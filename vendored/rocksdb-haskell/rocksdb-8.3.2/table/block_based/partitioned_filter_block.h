//  Copyright (c) 2011-present, Facebook, Inc.  All rights reserved.
//  This source code is licensed under both the GPLv2 (found in the
//  COPYING file in the root directory) and Apache 2.0 License
//  (found in the LICENSE.Apache file in the root directory).

#pragma once

#include <deque>
#include <list>
#include <string>
#include <unordered_map>

#include "block_cache.h"
#include "rocksdb/options.h"
#include "rocksdb/slice.h"
#include "rocksdb/slice_transform.h"
#include "table/block_based/block.h"
#include "table/block_based/filter_block_reader_common.h"
#include "table/block_based/full_filter_block.h"
#include "table/block_based/index_builder.h"
#include "util/autovector.h"
#include "util/hash_containers.h"

namespace ROCKSDB_NAMESPACE {
class InternalKeyComparator;

class PartitionedFilterBlockBuilder : public FullFilterBlockBuilder {
 public:
  explicit PartitionedFilterBlockBuilder(
      const SliceTransform* prefix_extractor, bool whole_key_filtering,
      FilterBitsBuilder* filter_bits_builder, int index_block_restart_interval,
      const bool use_value_delta_encoding,
      PartitionedIndexBuilder* const p_index_builder,
      const uint32_t partition_size);

  virtual ~PartitionedFilterBlockBuilder();

  void AddKey(const Slice& key) override;
  void Add(const Slice& key) override;
  size_t EstimateEntriesAdded() override;

  virtual Slice Finish(
      const BlockHandle& last_partition_block_handle, Status* status,
      std::unique_ptr<const char[]>* filter_data = nullptr) override;

  virtual void ResetFilterBitsBuilder() override {
    // Previously constructed partitioned filters by
    // this to-be-reset FiterBitsBuilder can also be
    // cleared
    filters.clear();
    FullFilterBlockBuilder::ResetFilterBitsBuilder();
  }

  // For PartitionFilter, optional post-verifing the filter is done
  // as part of PartitionFilterBlockBuilder::Finish
  // to avoid implementation complexity of doing it elsewhere.
  // Therefore we are skipping it in here.
  virtual Status MaybePostVerifyFilter(
      const Slice& /* filter_content */) override {
    return Status::OK();
  }

 private:
  // Filter data
  BlockBuilder index_on_filter_block_builder_;  // top-level index builder
  BlockBuilder
      index_on_filter_block_builder_without_seq_;  // same for user keys
  struct FilterEntry {
    std::string key;
    std::unique_ptr<const char[]> filter_data;
    Slice filter;
  };
  std::deque<FilterEntry> filters;  // list of partitioned filters and keys used
                                    // in building the index

  // Set to the first non-okay status if any of the filter
  // partitions experiences construction error.
  // If partitioned_filters_construction_status_ is non-okay,
  // then the whole partitioned filters should not be used.
  Status partitioned_filters_construction_status_;
  std::string last_filter_entry_key;
  std::unique_ptr<const char[]> last_filter_data;
  std::unique_ptr<IndexBuilder> value;
  bool finishing_filters =
      false;  // true if Finish is called once but not complete yet.
  // The policy of when cut a filter block and Finish it
  void MaybeCutAFilterBlock(const Slice* next_key);
  // Currently we keep the same number of partitions for filters and indexes.
  // This would allow for some potentioal optimizations in future. If such
  // optimizations did not realize we can use different number of partitions and
  // eliminate p_index_builder_
  PartitionedIndexBuilder* const p_index_builder_;
  // The desired number of keys per partition
  uint32_t keys_per_partition_;
  // The number of keys added to the last partition so far
  uint32_t keys_added_to_partition_;
  // According to the bits builders, how many keys/prefixes added
  // in all the filters we have fully built
  uint64_t total_added_in_built_;
  BlockHandle last_encoded_handle_;
};

class PartitionedFilterBlockReader
    : public FilterBlockReaderCommon<Block_kFilterPartitionIndex> {
 public:
  PartitionedFilterBlockReader(
      const BlockBasedTable* t,
      CachableEntry<Block_kFilterPartitionIndex>&& filter_block);

  static std::unique_ptr<FilterBlockReader> Create(
      const BlockBasedTable* table, const ReadOptions& ro,
      FilePrefetchBuffer* prefetch_buffer, bool use_cache, bool prefetch,
      bool pin, BlockCacheLookupContext* lookup_context);

  bool KeyMayMatch(const Slice& key, const bool no_io,
                   const Slice* const const_ikey_ptr, GetContext* get_context,
                   BlockCacheLookupContext* lookup_context,
                   const ReadOptions& read_options) override;
  void KeysMayMatch(MultiGetRange* range, const bool no_io,
                    BlockCacheLookupContext* lookup_context,
                    const ReadOptions& read_options) override;

  bool PrefixMayMatch(const Slice& prefix, const bool no_io,
                      const Slice* const const_ikey_ptr,
                      GetContext* get_context,
                      BlockCacheLookupContext* lookup_context,
                      const ReadOptions& read_options) override;
  void PrefixesMayMatch(MultiGetRange* range,
                        const SliceTransform* prefix_extractor,
                        const bool no_io,
                        BlockCacheLookupContext* lookup_context,
                        const ReadOptions& read_options) override;

  size_t ApproximateMemoryUsage() const override;

 private:
  BlockHandle GetFilterPartitionHandle(
      const CachableEntry<Block_kFilterPartitionIndex>& filter_block,
      const Slice& entry) const;
  Status GetFilterPartitionBlock(
      FilePrefetchBuffer* prefetch_buffer, const BlockHandle& handle,
      bool no_io, GetContext* get_context,
      BlockCacheLookupContext* lookup_context, const ReadOptions& read_options,
      CachableEntry<ParsedFullFilterBlock>* filter_block) const;

  using FilterFunction = bool (FullFilterBlockReader::*)(
      const Slice& slice, const bool no_io, const Slice* const const_ikey_ptr,
      GetContext* get_context, BlockCacheLookupContext* lookup_context,
      const ReadOptions& read_options);
  bool MayMatch(const Slice& slice, bool no_io, const Slice* const_ikey_ptr,
                GetContext* get_context,
                BlockCacheLookupContext* lookup_context,
                const ReadOptions& read_options,
                FilterFunction filter_function) const;
  using FilterManyFunction = void (FullFilterBlockReader::*)(
      MultiGetRange* range, const SliceTransform* prefix_extractor,
      const bool no_io, BlockCacheLookupContext* lookup_context,
      const ReadOptions& read_options);
  void MayMatch(MultiGetRange* range, const SliceTransform* prefix_extractor,
                bool no_io, BlockCacheLookupContext* lookup_context,
                const ReadOptions& read_options,
                FilterManyFunction filter_function) const;
  void MayMatchPartition(MultiGetRange* range,
                         const SliceTransform* prefix_extractor,
                         BlockHandle filter_handle, bool no_io,
                         BlockCacheLookupContext* lookup_context,
                         const ReadOptions& read_options,
                         FilterManyFunction filter_function) const;
  Status CacheDependencies(const ReadOptions& ro, bool pin,
                           FilePrefetchBuffer* tail_prefetch_buffer) override;

  const InternalKeyComparator* internal_comparator() const;
  bool index_key_includes_seq() const;
  bool index_value_is_full() const;

 protected:
  // For partition blocks pinned in cache. Can be a subset of blocks
  // in case some fail insertion on attempt to pin.
  UnorderedMap<uint64_t, CachableEntry<ParsedFullFilterBlock>> filter_map_;
};

}  // namespace ROCKSDB_NAMESPACE
