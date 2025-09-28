{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: P2P.Metrics
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Chainweb Dev Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- Prometheus metrics for P2P networking
--
module P2P.Metrics
( P2pMetrics(..)
, initP2pMetrics
, recordConnectionDuration
, recordBytesTransferred
, recordRequestDuration
, recordTaskQueueDepth
, updateActiveConnections
, updatePeerQualityScore
, incrementConnectionFailures
, incrementConnectionTimeouts
, collectMetrics

-- New labeled metric functions
, recordMessagePropagationDelay
, updatePeerGeographicDistribution
, updateMessageQueueDepth
, recordConnectionStateTransition
, updatePeerQualityScoreByPeer
, recordBandwidthUsageByPeer
, recordBandwidthUsageByMessageType
) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)

import Numeric.Natural

import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.Concurrent.Registry (Registry)
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import System.Metrics.Prometheus.Metric.Counter (Counter)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import System.Metrics.Prometheus.Metric.Histogram (Histogram)

-- -------------------------------------------------------------------------- --
-- P2P Metrics

data P2pMetrics = P2pMetrics
    { -- Existing metrics
      _p2pConnectionDuration :: !Histogram
    , _p2pActiveConnections :: !Gauge
    , _p2pBytesTransferred :: !Counter
    , _p2pRequestDuration :: !Histogram
    , _p2pPeerQualityScore :: !Gauge
    , _p2pTaskQueueDepth :: !Gauge
    , _p2pConnectionFailures :: !Counter
    , _p2pConnectionTimeouts :: !Counter
    , _p2pSessionSuccesses :: !Counter
    , _p2pSessionFailures :: !Counter

    -- New metrics with labels
    , _p2pMessagePropagationDelay :: !Histogram  -- with message_type labels
    , _p2pPeerGeographicDistribution :: !Gauge   -- with country/region labels
    , _p2pMessageQueueDepth :: !Gauge            -- with message_type labels
    , _p2pConnectionStateTransitions :: !Counter -- with state labels
    , _p2pPeerQualityScoreByPeer :: !Gauge       -- with peer_id labels
    , _p2pBandwidthUsageByPeer :: !Counter       -- with peer_id labels
    , _p2pBandwidthUsageByMessageType :: !Counter -- with message_type labels

    -- Store label-specific metrics in HashMaps for efficient lookup
    , _p2pMetricsWithLabels :: !(HashMap T.Text (HashMap T.Text Counter))
    , _p2pGaugesWithLabels :: !(HashMap T.Text (HashMap T.Text Gauge))
    , _p2pHistogramsWithLabels :: !(HashMap T.Text (HashMap T.Text Histogram))

    , _p2pRegistry :: !Registry
    }

-- | Initialize P2P metrics with the global Prometheus registry
initP2pMetrics :: IO P2pMetrics
initP2pMetrics = do
    _p2pRegistry <- Registry.new

    -- Existing metrics
    _p2pConnectionDuration <- Registry.registerHistogram "chainweb_p2p_connection_duration_seconds" mempty defaultBuckets _p2pRegistry
    _p2pActiveConnections <- Registry.registerGauge "chainweb_p2p_active_connections" mempty _p2pRegistry
    _p2pBytesTransferred <- Registry.registerCounter "chainweb_p2p_bytes_transferred_total" mempty _p2pRegistry
    _p2pRequestDuration <- Registry.registerHistogram "chainweb_p2p_request_duration_seconds" mempty defaultBuckets _p2pRegistry
    _p2pPeerQualityScore <- Registry.registerGauge "chainweb_p2p_peer_quality_score" mempty _p2pRegistry
    _p2pTaskQueueDepth <- Registry.registerGauge "chainweb_p2p_task_queue_depth" mempty _p2pRegistry
    _p2pConnectionFailures <- Registry.registerCounter "chainweb_p2p_connection_failures_total" mempty _p2pRegistry
    _p2pConnectionTimeouts <- Registry.registerCounter "chainweb_p2p_connection_timeouts_total" mempty _p2pRegistry
    _p2pSessionSuccesses <- Registry.registerCounter "chainweb_p2p_session_successes_total" mempty _p2pRegistry
    _p2pSessionFailures <- Registry.registerCounter "chainweb_p2p_session_failures_total" mempty _p2pRegistry

    -- New labeled metrics (using base metrics without labels for now)
    _p2pMessagePropagationDelay <- Registry.registerHistogram "chainweb_p2p_message_propagation_delay_seconds" mempty propagationBuckets _p2pRegistry
    _p2pPeerGeographicDistribution <- Registry.registerGauge "chainweb_p2p_peer_geographic_distribution" mempty _p2pRegistry
    _p2pMessageQueueDepth <- Registry.registerGauge "chainweb_p2p_message_queue_depth" mempty _p2pRegistry
    _p2pConnectionStateTransitions <- Registry.registerCounter "chainweb_p2p_connection_state_transitions_total" mempty _p2pRegistry
    _p2pPeerQualityScoreByPeer <- Registry.registerGauge "chainweb_p2p_peer_quality_score_by_peer" mempty _p2pRegistry
    _p2pBandwidthUsageByPeer <- Registry.registerCounter "chainweb_p2p_bandwidth_usage_by_peer_bytes_total" mempty _p2pRegistry
    _p2pBandwidthUsageByMessageType <- Registry.registerCounter "chainweb_p2p_bandwidth_usage_by_message_type_bytes_total" mempty _p2pRegistry

    -- Initialize empty HashMaps for labeled metrics
    let _p2pMetricsWithLabels = HM.empty
    let _p2pGaugesWithLabels = HM.empty
    let _p2pHistogramsWithLabels = HM.empty

    pure P2pMetrics {..}
  where
    defaultBuckets = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]
    propagationBuckets = [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 30, 60]

-- -------------------------------------------------------------------------- --
-- Metric Recording Functions

recordConnectionDuration :: P2pMetrics -> Double -> IO ()
recordConnectionDuration P2pMetrics{..} duration =
    void $ Histogram.observe duration _p2pConnectionDuration

recordBytesTransferred :: P2pMetrics -> Natural -> IO ()
recordBytesTransferred P2pMetrics{..} bytes =
    void $ Counter.add (fromIntegral bytes) _p2pBytesTransferred

recordRequestDuration :: P2pMetrics -> Double -> IO ()
recordRequestDuration P2pMetrics{..} duration =
    void $ Histogram.observe duration _p2pRequestDuration

recordTaskQueueDepth :: P2pMetrics -> Natural -> IO ()
recordTaskQueueDepth P2pMetrics{..} depth =
    Gauge.set (fromIntegral depth) _p2pTaskQueueDepth

updateActiveConnections :: P2pMetrics -> Natural -> IO ()
updateActiveConnections P2pMetrics{..} count =
    Gauge.set (fromIntegral count) _p2pActiveConnections

updatePeerQualityScore :: P2pMetrics -> Double -> IO ()
updatePeerQualityScore P2pMetrics{..} score =
    Gauge.set score _p2pPeerQualityScore

incrementConnectionFailures :: P2pMetrics -> IO ()
incrementConnectionFailures P2pMetrics{..} =
    void $ Counter.inc _p2pConnectionFailures

incrementConnectionTimeouts :: P2pMetrics -> IO ()
incrementConnectionTimeouts P2pMetrics{..} =
    void $ Counter.inc _p2pConnectionTimeouts

-- -------------------------------------------------------------------------- --
-- New Labeled Metric Recording Functions

-- | Record message propagation delay with message type label
recordMessagePropagationDelay :: P2pMetrics -> T.Text -> Double -> IO ()
recordMessagePropagationDelay P2pMetrics{..} _messageType delay =
    -- For now, record to the base histogram (labels will be added in future enhancement)
    void $ Histogram.observe delay _p2pMessagePropagationDelay

-- | Update peer geographic distribution with country/region label
updatePeerGeographicDistribution :: P2pMetrics -> T.Text -> Natural -> IO ()
updatePeerGeographicDistribution P2pMetrics{..} _country count =
    -- For now, record to the base gauge (labels will be added in future enhancement)
    Gauge.set (fromIntegral count) _p2pPeerGeographicDistribution

-- | Update message queue depth with message type label
updateMessageQueueDepth :: P2pMetrics -> T.Text -> Natural -> IO ()
updateMessageQueueDepth P2pMetrics{..} _messageType depth =
    -- For now, record to the base gauge (labels will be added in future enhancement)
    Gauge.set (fromIntegral depth) _p2pMessageQueueDepth

-- | Record connection state transition with state labels
recordConnectionStateTransition :: P2pMetrics -> T.Text -> T.Text -> IO ()
recordConnectionStateTransition P2pMetrics{..} _fromState _toState =
    -- For now, record to the base counter (labels will be added in future enhancement)
    void $ Counter.inc _p2pConnectionStateTransitions

-- | Update peer quality score by peer ID
updatePeerQualityScoreByPeer :: P2pMetrics -> T.Text -> Double -> IO ()
updatePeerQualityScoreByPeer P2pMetrics{..} _peerId score =
    -- For now, record to the base gauge (labels will be added in future enhancement)
    Gauge.set score _p2pPeerQualityScoreByPeer

-- | Record bandwidth usage by peer ID
recordBandwidthUsageByPeer :: P2pMetrics -> T.Text -> Natural -> IO ()
recordBandwidthUsageByPeer P2pMetrics{..} _peerId bytes =
    -- For now, record to the base counter (labels will be added in future enhancement)
    void $ Counter.add (fromIntegral bytes) _p2pBandwidthUsageByPeer

-- | Record bandwidth usage by message type
recordBandwidthUsageByMessageType :: P2pMetrics -> T.Text -> Natural -> IO ()
recordBandwidthUsageByMessageType P2pMetrics{..} _messageType bytes =
    -- For now, record to the base counter (labels will be added in future enhancement)
    void $ Counter.add (fromIntegral bytes) _p2pBandwidthUsageByMessageType

-- -------------------------------------------------------------------------- --
-- Metrics Collection

-- | Collect all P2P metrics into a text format
collectMetrics :: P2pMetrics -> IO T.Text
collectMetrics P2pMetrics{..} = do
    -- Sample individual metrics and format as basic Prometheus text
    activeConn <- Gauge.sample _p2pActiveConnections
    bytesTransferred <- Counter.sample _p2pBytesTransferred
    qualityScore <- Gauge.sample _p2pPeerQualityScore
    queueDepth <- Gauge.sample _p2pTaskQueueDepth
    failures <- Counter.sample _p2pConnectionFailures
    timeouts <- Counter.sample _p2pConnectionTimeouts
    successes <- Counter.sample _p2pSessionSuccesses
    sessionFailures <- Counter.sample _p2pSessionFailures

    -- Sample new labeled metrics (base values for now)
    geoDistribution <- Gauge.sample _p2pPeerGeographicDistribution
    messageQueueDepth <- Gauge.sample _p2pMessageQueueDepth
    stateTransitions <- Counter.sample _p2pConnectionStateTransitions
    peerQualityByPeer <- Gauge.sample _p2pPeerQualityScoreByPeer
    bandwidthByPeer <- Counter.sample _p2pBandwidthUsageByPeer
    bandwidthByMessageType <- Counter.sample _p2pBandwidthUsageByMessageType

    pure $ T.unlines
        [ "# HELP chainweb_p2p_active_connections Number of active P2P connections"
        , "# TYPE chainweb_p2p_active_connections gauge"
        , "chainweb_p2p_active_connections " <> T.pack (show activeConn)
        , "# HELP chainweb_p2p_bytes_transferred_total Total bytes transferred over P2P"
        , "# TYPE chainweb_p2p_bytes_transferred_total counter"
        , "chainweb_p2p_bytes_transferred_total " <> T.pack (show bytesTransferred)
        , "# HELP chainweb_p2p_peer_quality_score Quality score of peers"
        , "# TYPE chainweb_p2p_peer_quality_score gauge"
        , "chainweb_p2p_peer_quality_score " <> T.pack (show qualityScore)
        , "# HELP chainweb_p2p_task_queue_depth Depth of P2P task queue"
        , "# TYPE chainweb_p2p_task_queue_depth gauge"
        , "chainweb_p2p_task_queue_depth " <> T.pack (show queueDepth)
        , "# HELP chainweb_p2p_connection_failures_total Total P2P connection failures"
        , "# TYPE chainweb_p2p_connection_failures_total counter"
        , "chainweb_p2p_connection_failures_total " <> T.pack (show failures)
        , "# HELP chainweb_p2p_connection_timeouts_total Total P2P connection timeouts"
        , "# TYPE chainweb_p2p_connection_timeouts_total counter"
        , "chainweb_p2p_connection_timeouts_total " <> T.pack (show timeouts)
        , "# HELP chainweb_p2p_session_successes_total Total successful P2P sessions"
        , "# TYPE chainweb_p2p_session_successes_total counter"
        , "chainweb_p2p_session_successes_total " <> T.pack (show successes)
        , "# HELP chainweb_p2p_session_failures_total Total failed P2P sessions"
        , "# TYPE chainweb_p2p_session_failures_total counter"
        , "chainweb_p2p_session_failures_total " <> T.pack (show sessionFailures)

        -- New labeled metrics
        , "# HELP chainweb_p2p_peer_geographic_distribution Distribution of peers by geographic location"
        , "# TYPE chainweb_p2p_peer_geographic_distribution gauge"
        , "chainweb_p2p_peer_geographic_distribution " <> T.pack (show geoDistribution)
        , "# HELP chainweb_p2p_message_queue_depth Queue depth by message type"
        , "# TYPE chainweb_p2p_message_queue_depth gauge"
        , "chainweb_p2p_message_queue_depth " <> T.pack (show messageQueueDepth)
        , "# HELP chainweb_p2p_connection_state_transitions_total Connection state transitions"
        , "# TYPE chainweb_p2p_connection_state_transitions_total counter"
        , "chainweb_p2p_connection_state_transitions_total " <> T.pack (show stateTransitions)
        , "# HELP chainweb_p2p_peer_quality_score_by_peer Peer quality scores by peer ID"
        , "# TYPE chainweb_p2p_peer_quality_score_by_peer gauge"
        , "chainweb_p2p_peer_quality_score_by_peer " <> T.pack (show peerQualityByPeer)
        , "# HELP chainweb_p2p_bandwidth_usage_by_peer_bytes_total Bandwidth usage by peer"
        , "# TYPE chainweb_p2p_bandwidth_usage_by_peer_bytes_total counter"
        , "chainweb_p2p_bandwidth_usage_by_peer_bytes_total " <> T.pack (show bandwidthByPeer)
        , "# HELP chainweb_p2p_bandwidth_usage_by_message_type_bytes_total Bandwidth usage by message type"
        , "# TYPE chainweb_p2p_bandwidth_usage_by_message_type_bytes_total counter"
        , "chainweb_p2p_bandwidth_usage_by_message_type_bytes_total " <> T.pack (show bandwidthByMessageType)
        ]

