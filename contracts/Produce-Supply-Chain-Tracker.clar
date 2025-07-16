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
        current-holder: principal,
    }
)

(define-map TraceHistory
    {
        batch-id: uint,
        trace-id: uint,
    }
    {
        timestamp: uint,
        location: (string-ascii 50),
        handler: principal,
        status: (string-ascii 20),
        temperature: uint,
        humidity: uint,
    }
)

(define-data-var last-batch-id uint u0)
(define-data-var last-trace-id uint u0)

(define-public (register-batch
        (harvest-location (string-ascii 50))
        (produce-type (string-ascii 30))
        (quantity uint)
    )
    (let ((batch-id (+ (var-get last-batch-id) u1)))
        (try! (nft-mint? produce-batch batch-id tx-sender))
        (map-set BatchDetails { batch-id: batch-id } {
            producer: tx-sender,
            harvest-date: stacks-block-height,
            harvest-location: harvest-location,
            produce-type: produce-type,
            quantity: quantity,
            current-status: "harvested",
            current-holder: tx-sender,
        })
        (var-set last-batch-id batch-id)
        (map-set TraceHistory {
            batch-id: batch-id,
            trace-id: u1,
        } {
            timestamp: stacks-block-height,
            location: harvest-location,
            handler: tx-sender,
            status: "harvested",
            temperature: u20,
            humidity: u60,
        })
        (var-set last-trace-id u1)
        (ok batch-id)
    )
)
(define-public (register-batch-v2
        (harvest-location (string-ascii 50))
        (produce-type (string-ascii 30))
        (quantity uint)
    )
    (let (
            (batch-id (+ (var-get last-batch-id) u1))
            (trace-id (+ (var-get last-trace-id) u1))
        )
        (try! (nft-mint? produce-batch batch-id tx-sender))
        (map-set BatchDetails { batch-id: batch-id } {
            producer: tx-sender,
            harvest-date: burn-block-height,
            harvest-location: harvest-location,
            produce-type: produce-type,
            quantity: quantity,
            current-status: "harvested",
            current-holder: tx-sender,
        })
        (var-set last-batch-id batch-id)
        (map-set TraceHistory {
            batch-id: batch-id,
            trace-id: trace-id,
        } {
            timestamp: stacks-block-height,
            location: harvest-location,
            handler: tx-sender,
            status: "harvested",
            temperature: u20,
            humidity: u60,
        })
        (var-set last-trace-id trace-id)
        (ok batch-id)
    )
)
(define-public (add-trace-event
        (batch-id uint)
        (location (string-ascii 50))
        (status (string-ascii 20))
        (temperature uint)
        (humidity uint)
    )
    (let ((trace-id (+ (var-get last-trace-id) u1)))
        (map-set TraceHistory {
            batch-id: batch-id,
            trace-id: trace-id,
        } {
            timestamp: stacks-block-height,
            location: location,
            handler: tx-sender,
            status: status,
            temperature: temperature,
            humidity: humidity,
        })
        (var-set last-trace-id trace-id)
        (ok true)
    )
)

(define-read-only (get-batch-details (batch-id uint))
    (ok (unwrap! (map-get? BatchDetails { batch-id: batch-id }) err-batch-not-found))
)

(define-read-only (get-trace-event
        (batch-id uint)
        (trace-id uint)
    )
    (ok (unwrap!
        (map-get? TraceHistory {
            batch-id: batch-id,
            trace-id: trace-id,
        })
        err-batch-not-found
    ))
)

(define-map QualityCertificates
    {
        batch-id: uint,
        cert-id: uint,
    }
    {
        inspector: principal,
        certification-type: (string-ascii 30),
        grade: (string-ascii 10),
        issue-date: uint,
        expiry-date: uint,
        is-valid: bool,
        notes: (string-ascii 100),
    }
)

(define-map AuthorizedInspectors
    { inspector: principal }
    {
        authorized: bool,
        certification-types: (list 5 (string-ascii 30)),
    }
)

(define-data-var last-cert-id uint u0)

(define-public (authorize-inspector
        (inspector principal)
        (cert-types (list 5 (string-ascii 30)))
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set AuthorizedInspectors { inspector: inspector } {
            authorized: true,
            certification-types: cert-types,
        })
        (ok true)
    )
)

(define-public (issue-certificate
        (batch-id uint)
        (certification-type (string-ascii 30))
        (grade (string-ascii 10))
        (expiry-blocks uint)
        (notes (string-ascii 100))
    )
    (let (
            (cert-id (+ (var-get last-cert-id) u1))
            (inspector-data (unwrap! (map-get? AuthorizedInspectors { inspector: tx-sender })
                err-not-authorized
            ))
        )
        (asserts! (get authorized inspector-data) err-not-authorized)
        (asserts! (is-some (map-get? BatchDetails { batch-id: batch-id }))
            err-batch-not-found
        )
        (map-set QualityCertificates {
            batch-id: batch-id,
            cert-id: cert-id,
        } {
            inspector: tx-sender,
            certification-type: certification-type,
            grade: grade,
            issue-date: stacks-block-height,
            expiry-date: (+ stacks-block-height expiry-blocks),
            is-valid: true,
            notes: notes,
        })
        (var-set last-cert-id cert-id)
        (ok cert-id)
    )
)

(define-public (revoke-certificate
        (batch-id uint)
        (cert-id uint)
    )
    (let ((cert-data (unwrap!
            (map-get? QualityCertificates {
                batch-id: batch-id,
                cert-id: cert-id,
            })
            err-batch-not-found
        )))
        (asserts!
            (or (is-eq tx-sender (get inspector cert-data)) (is-eq tx-sender contract-owner))
            err-not-authorized
        )
        (map-set QualityCertificates {
            batch-id: batch-id,
            cert-id: cert-id,
        }
            (merge cert-data { is-valid: false })
        )
        (ok true)
    )
)

(define-read-only (get-certificate
        (batch-id uint)
        (cert-id uint)
    )
    (ok (unwrap!
        (map-get? QualityCertificates {
            batch-id: batch-id,
            cert-id: cert-id,
        })
        err-batch-not-found
    ))
)

(define-read-only (is-certificate-valid
        (batch-id uint)
        (cert-id uint)
    )
    (let ((cert-data (unwrap!
            (map-get? QualityCertificates {
                batch-id: batch-id,
                cert-id: cert-id,
            })
            err-batch-not-found
        )))
        (ok (and
            (get is-valid cert-data)
            (< stacks-block-height (get expiry-date cert-data))
        ))
    )
)

(define-map TransferRequests
    {
        batch-id: uint,
        request-id: uint,
    }
    {
        from: principal,
        to: principal,
        requested-at: uint,
        status: (string-ascii 20),
        transfer-type: (string-ascii 30),
        notes: (string-ascii 100),
    }
)

(define-map TransferHistory
    {
        batch-id: uint,
        transfer-id: uint,
    }
    {
        from: principal,
        to: principal,
        transferred-at: uint,
        transfer-type: (string-ascii 30),
        handler: principal,
    }
)

(define-data-var last-request-id uint u0)
(define-data-var last-transfer-id uint u0)

(define-public (request-transfer
        (batch-id uint)
        (to principal)
        (transfer-type (string-ascii 30))
        (notes (string-ascii 100))
    )
    (let (
            (request-id (+ (var-get last-request-id) u1))
            (batch-data (unwrap! (map-get? BatchDetails { batch-id: batch-id })
                err-batch-not-found
            ))
        )
        (asserts! (is-eq tx-sender (get current-holder batch-data))
            err-not-authorized
        )
        (map-set TransferRequests {
            batch-id: batch-id,
            request-id: request-id,
        } {
            from: tx-sender,
            to: to,
            requested-at: stacks-block-height,
            status: "pending",
            transfer-type: transfer-type,
            notes: notes,
        })
        (var-set last-request-id request-id)
        (ok request-id)
    )
)

(define-public (accept-transfer
        (batch-id uint)
        (request-id uint)
    )
    (let (
            (request-data (unwrap!
                (map-get? TransferRequests {
                    batch-id: batch-id,
                    request-id: request-id,
                })
                err-batch-not-found
            ))
            (batch-data (unwrap! (map-get? BatchDetails { batch-id: batch-id })
                err-batch-not-found
            ))
            (transfer-id (+ (var-get last-transfer-id) u1))
        )
        (asserts! (is-eq tx-sender (get to request-data)) err-not-authorized)
        (asserts! (is-eq (get status request-data) "pending") err-invalid-status)
        (try! (nft-transfer? produce-batch batch-id (get current-holder batch-data)
            tx-sender
        ))
        (map-set BatchDetails { batch-id: batch-id } {
            producer: (get producer batch-data),
            harvest-date: (get harvest-date batch-data),
            harvest-location: (get harvest-location batch-data),
            produce-type: (get produce-type batch-data),
            quantity: (get quantity batch-data),
            current-status: "in-transit",
            current-holder: tx-sender,
        })
        (map-set TransferRequests {
            batch-id: batch-id,
            request-id: request-id,
        }
            (merge request-data { status: "completed" })
        )
        (map-set TransferHistory {
            batch-id: batch-id,
            transfer-id: transfer-id,
        } {
            from: (get from request-data),
            to: tx-sender,
            transferred-at: stacks-block-height,
            transfer-type: (get transfer-type request-data),
            handler: tx-sender,
        })
        (var-set last-transfer-id transfer-id)
        (ok transfer-id)
    )
)

(define-public (reject-transfer
        (batch-id uint)
        (request-id uint)
    )
    (let ((request-data (unwrap!
            (map-get? TransferRequests {
                batch-id: batch-id,
                request-id: request-id,
            })
            err-batch-not-found
        )))
        (asserts! (is-eq tx-sender (get to request-data)) err-not-authorized)
        (asserts! (is-eq (get status request-data) "pending") err-invalid-status)
        (map-set TransferRequests {
            batch-id: batch-id,
            request-id: request-id,
        }
            (merge request-data { status: "rejected" })
        )
        (ok true)
    )
)

(define-read-only (get-transfer-request
        (batch-id uint)
        (request-id uint)
    )
    (ok (unwrap!
        (map-get? TransferRequests {
            batch-id: batch-id,
            request-id: request-id,
        })
        err-batch-not-found
    ))
)

(define-read-only (get-transfer-history
        (batch-id uint)
        (transfer-id uint)
    )
    (ok (unwrap!
        (map-get? TransferHistory {
            batch-id: batch-id,
            transfer-id: transfer-id,
        })
        err-batch-not-found
    ))
)

(define-read-only (get-batch-owner (batch-id uint))
    (let ((batch-data (unwrap! (map-get? BatchDetails { batch-id: batch-id })
            err-batch-not-found
        )))
        (ok (get current-holder batch-data))
    )
)
