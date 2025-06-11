(define-non-fungible-token produce-batch uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-batch-exists (err u101))
(define-constant err-batch-not-found (err u102))
(define-constant err-invalid-status (err u103))

(define-map BatchDetails
  { batch-id: uint }
  {
    producer: principal,
    harvest-date: uint,
    harvest-location: (string-ascii 50),
    produce-type: (string-ascii 30),
    quantity: uint,
    current-status: (string-ascii 20),
    current-holder: principal
  }
)

(define-map TraceHistory
  { batch-id: uint, trace-id: uint }
  {
    timestamp: uint,
    location: (string-ascii 50),
    handler: principal,
    status: (string-ascii 20),
    temperature: uint,
    humidity: uint
  }
)

(define-data-var last-batch-id uint u0)
(define-data-var last-trace-id uint u0)

(define-public (register-batch 
    (harvest-location (string-ascii 50))
    (produce-type (string-ascii 30))
    (quantity uint))
  (let
    ((batch-id (+ (var-get last-batch-id) u1)))
    (try! (nft-mint? produce-batch batch-id tx-sender))
    (map-set BatchDetails
      { batch-id: batch-id }
      {
        producer: tx-sender,
        harvest-date: stacks-block-height,
        harvest-location: harvest-location,
        produce-type: produce-type,
        quantity: quantity,
        current-status: "harvested",
        current-holder: tx-sender
      }
    )
    (var-set last-batch-id batch-id)
    (map-set TraceHistory
      { batch-id: batch-id, trace-id: u1 }
      {
        timestamp: stacks-block-height,
        location: harvest-location,
        handler: tx-sender,
        status: "harvested",
        temperature: u20,
        humidity: u60
      }
    )
    (var-set last-trace-id u1)
    (ok batch-id)
  )
)
(define-public (register-batch-v2 
    (harvest-location (string-ascii 50))
    (produce-type (string-ascii 30))
    (quantity uint))
  (let
    ((batch-id (+ (var-get last-batch-id) u1))
     (trace-id (+ (var-get last-trace-id) u1)))
    (try! (nft-mint? produce-batch batch-id tx-sender))
    (map-set BatchDetails
      { batch-id: batch-id }
      {
        producer: tx-sender,
        harvest-date: burn-block-height,
        harvest-location: harvest-location,
        produce-type: produce-type,
        quantity: quantity,
        current-status: "harvested",
        current-holder: tx-sender
      }
    )
    (var-set last-batch-id batch-id)
    (map-set TraceHistory
      { batch-id: batch-id, trace-id: trace-id }
      {
        timestamp: stacks-block-height,
        location: harvest-location,
        handler: tx-sender,
        status: "harvested",
        temperature: u20,
        humidity: u60
      }
    )
    (var-set last-trace-id trace-id)
    (ok batch-id)
  ))
(define-public (add-trace-event
    (batch-id uint)
    (location (string-ascii 50))
    (status (string-ascii 20))
    (temperature uint)
    (humidity uint))
  (let
    ((trace-id (+ (var-get last-trace-id) u1)))
    (map-set TraceHistory
      { batch-id: batch-id, trace-id: trace-id }
      {
        timestamp: stacks-block-height,
        location: location,
        handler: tx-sender,
        status: status,
        temperature: temperature,
        humidity: humidity
      }
    )
    (var-set last-trace-id trace-id)
    (ok true)
  )
)

(define-read-only (get-batch-details (batch-id uint))
  (ok (unwrap! (map-get? BatchDetails { batch-id: batch-id }) err-batch-not-found))
)

(define-read-only (get-trace-event (batch-id uint) (trace-id uint))
  (ok (unwrap! (map-get? TraceHistory { batch-id: batch-id, trace-id: trace-id }) err-batch-not-found))
)
