# 🌾 Produce Supply Chain Tracker

A blockchain-based solution for transparent and efficient produce supply chain management.

## 🎯 Overview

This smart contract enables tracking of produce from harvest to retail, ensuring transparency and traceability in the agricultural supply chain.

## ✨ Features

- 🏷️ NFT-based batch tracking
- 📍 Location and condition monitoring
- 🔄 Transfer management
- 📊 Complete trace history
- 🌡️ Temperature and humidity logging

## 🚀 Contract Functions

### Register Batch
```clarity
(register-batch harvest-location produce-type quantity)
```
Creates a new batch of produce with harvest details.

### Transfer Batch
```clarity
(transfer-batch batch-id recipient location status)
```
Transfers batch ownership and updates status.

### Add Trace Event
```clarity
(add-trace-event batch-id location status temperature humidity)
```
Logs a new trace event with environmental conditions.

### Read Functions
```clarity
(get-batch-details batch-id)
(get-trace-event batch-id trace-id)
```
Retrieve batch information and trace history.

## 🔧 Usage

1. Deploy the contract
2. Register produce batches at harvest
3. Track transfers through the supply chain
4. Monitor conditions with trace events
5. Verify authenticity and history using batch IDs

## 📝 License

MIT
```
