#!/bin/bash

dist/build/transaction-generator/transaction-generator --config-file examples/local-transaction-templates/deploy-contracts-template.yaml \
    && sleep 2s \
    && dist/build/transaction-generator/transaction-generator --config-file examples/local-transaction-templates/run-standard-contracts-template.yaml
