(define-non-fungible-token produce-batch uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-batch-exists (err u101))
(define-constant err-batch-not-found (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-batch-recalled (err u104))
(define-constant err-insufficient-quantity (err u105))
(define-constant err-incompatible-batches (err u106))
(define-constant err-invalid-split (err u107))
(define-constant err-batch-expired (err u108))
(define-constant err-expiration-exists (err u109))

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

(define-map BatchRecalls
    {
        batch-id: uint,
        recall-id: uint,
    }
    {
        initiator: principal,
        reason: (string-ascii 200),
        severity: (string-ascii 20),
        initiated-at: uint,
        is-active: bool,
        affected-parties: (list 10 principal),
    }
)

(define-map RecallNotifications
    { 
        batch-id: uint,
        notified-party: principal,
    }
    {
        recall-id: uint,
        notified-at: uint,
        acknowledgement: bool,
    }
)

(define-data-var last-recall-id uint u0)

(define-public (initiate-recall
        (batch-id uint)
        (reason (string-ascii 200))
        (severity (string-ascii 20))
        (affected-parties (list 10 principal))
    )
    (let (
            (recall-id (+ (var-get last-recall-id) u1))
            (batch-data (unwrap! (map-get? BatchDetails { batch-id: batch-id })
                err-batch-not-found
            ))
        )
        (asserts! 
            (or 
                (is-eq tx-sender contract-owner)
                (is-eq tx-sender (get producer batch-data))
                (is-eq tx-sender (get current-holder batch-data))
            )
            err-not-authorized
        )
        (map-set BatchRecalls {
            batch-id: batch-id,
            recall-id: recall-id,
        } {
            initiator: tx-sender,
            reason: reason,
            severity: severity,
            initiated-at: stacks-block-height,
            is-active: true,
            affected-parties: affected-parties,
        })
        (map-set BatchDetails { batch-id: batch-id } {
            producer: (get producer batch-data),
            harvest-date: (get harvest-date batch-data),
            harvest-location: (get harvest-location batch-data),
            produce-type: (get produce-type batch-data),
            quantity: (get quantity batch-data),
            current-status: "recalled",
            current-holder: (get current-holder batch-data),
        })
        (var-set last-recall-id recall-id)
        (ok recall-id)
    )
)

(define-public (acknowledge-recall
        (batch-id uint)
        (recall-id uint)
    )
    (let ((recall-data (unwrap!
            (map-get? BatchRecalls {
                batch-id: batch-id,
                recall-id: recall-id,
            })
            err-batch-not-found
        )))
        (asserts! (get is-active recall-data) err-invalid-status)
        (map-set RecallNotifications {
            batch-id: batch-id,
            notified-party: tx-sender,
        } {
            recall-id: recall-id,
            notified-at: stacks-block-height,
            acknowledgement: true,
        })
        (ok true)
    )
)

(define-public (close-recall
        (batch-id uint)
        (recall-id uint)
    )
    (let ((recall-data (unwrap!
            (map-get? BatchRecalls {
                batch-id: batch-id,
                recall-id: recall-id,
            })
            err-batch-not-found
        )))
        (asserts! 
            (or 
                (is-eq tx-sender contract-owner)
                (is-eq tx-sender (get initiator recall-data))
            )
            err-not-authorized
        )
        (asserts! (get is-active recall-data) err-invalid-status)
        (map-set BatchRecalls {
            batch-id: batch-id,
            recall-id: recall-id,
        }
            (merge recall-data { is-active: false })
        )
        (ok true)
    )
)

(define-read-only (get-recall-details
        (batch-id uint)
        (recall-id uint)
    )
    (ok (unwrap!
        (map-get? BatchRecalls {
            batch-id: batch-id,
            recall-id: recall-id,
        })
        err-batch-not-found
    ))
)

(define-read-only (is-batch-recalled (batch-id uint))
    (let ((batch-data (unwrap! (map-get? BatchDetails { batch-id: batch-id })
            err-batch-not-found
        )))
        (ok (is-eq (get current-status batch-data) "recalled"))
    )
)

(define-read-only (get-recall-notification
        (batch-id uint)
        (notified-party principal)
    )
    (ok (unwrap!
        (map-get? RecallNotifications {
            batch-id: batch-id,
            notified-party: notified-party,
        })
        err-batch-not-found
    ))
)

(define-map BatchSplitHistory
    {
        parent-batch-id: uint,
        split-id: uint,
    }
    {
        operator: principal,
        split-at: uint,
        child-batches: (list 10 uint),
        split-quantities: (list 10 uint),
        reason: (string-ascii 100),
    }
)

(define-map BatchMergeHistory
    {
        merge-id: uint,
    }
    {
        operator: principal,
        merged-at: uint,
        source-batches: (list 10 uint),
        target-batch-id: uint,
        total-quantity: uint,
        reason: (string-ascii 100),
    }
)

(define-data-var last-split-id uint u0)
(define-data-var last-merge-id uint u0)

(define-public (split-batch
        (parent-batch-id uint)
        (split-quantities (list 10 uint))
        (reason (string-ascii 100))
    )
    (let (
            (parent-batch (unwrap! (map-get? BatchDetails { batch-id: parent-batch-id })
                err-batch-not-found
            ))
            (split-id (+ (var-get last-split-id) u1))
            (total-split-qty (fold + split-quantities u0))
            (parent-qty (get quantity parent-batch))
        )
        (asserts! (is-eq tx-sender (get current-holder parent-batch))
            err-not-authorized
        )
        (asserts! (< u0 (len split-quantities)) err-invalid-split)
        (asserts! (< (len split-quantities) u11) err-invalid-split)
        (asserts! (is-eq total-split-qty parent-qty) err-insufficient-quantity)
        (asserts! (not (is-eq (get current-status parent-batch) "recalled"))
            err-batch-recalled
        )
        (let ((child-batch-ids (create-child-batches parent-batch split-quantities)))
            (try! (nft-burn? produce-batch parent-batch-id tx-sender))
            (map-set BatchSplitHistory {
                parent-batch-id: parent-batch-id,
                split-id: split-id,
            } {
                operator: tx-sender,
                split-at: stacks-block-height,
                child-batches: child-batch-ids,
                split-quantities: split-quantities,
                reason: reason,
            })
            (var-set last-split-id split-id)
            (ok child-batch-ids)
        )
    )
)

(define-private (create-child-batches 
        (parent-batch { producer: principal, harvest-date: uint, harvest-location: (string-ascii 50), produce-type: (string-ascii 30), quantity: uint, current-status: (string-ascii 20), current-holder: principal })
        (quantities (list 10 uint))
    )
    (begin
        (let ((result (fold create-child-batch-helper quantities { batch-ids: (list), parent: parent-batch, last-id: (var-get last-batch-id) })))
            (var-set last-batch-id (get last-id result))
            (get batch-ids result)
        )
    )
)

(define-private (create-child-batch-helper 
        (quantity uint)
        (acc { batch-ids: (list 10 uint), parent: { producer: principal, harvest-date: uint, harvest-location: (string-ascii 50), produce-type: (string-ascii 30), quantity: uint, current-status: (string-ascii 20), current-holder: principal }, last-id: uint })
    )
    (let ((new-batch-id (+ (get last-id acc) u1)))
        (unwrap-panic (nft-mint? produce-batch new-batch-id tx-sender))
        (map-set BatchDetails { batch-id: new-batch-id } {
            producer: (get producer (get parent acc)),
            harvest-date: (get harvest-date (get parent acc)),
            harvest-location: (get harvest-location (get parent acc)),
            produce-type: (get produce-type (get parent acc)),
            quantity: quantity,
            current-status: "split",
            current-holder: tx-sender,
        })
        {
            batch-ids: (unwrap-panic (as-max-len? (append (get batch-ids acc) new-batch-id) u10)),
            parent: (get parent acc),
            last-id: new-batch-id
        }
    )
)

(define-public (merge-batches
        (source-batch-ids (list 10 uint))
        (reason (string-ascii 100))
    )
    (let (
            (merge-id (+ (var-get last-merge-id) u1))
            (target-batch-id (+ (var-get last-batch-id) u1))
            (first-batch (unwrap! (map-get? BatchDetails { batch-id: (unwrap-panic (element-at source-batch-ids u0)) })
                err-batch-not-found
            ))
        )
        (asserts! (< u1 (len source-batch-ids)) err-invalid-split)
        (asserts! (< (len source-batch-ids) u11) err-invalid-split)
        (asserts! (validate-merge-compatibility source-batch-ids first-batch) err-incompatible-batches)
        (let ((total-quantity (calculate-total-quantity source-batch-ids)))
            (try! (burn-source-batches source-batch-ids))
            (try! (nft-mint? produce-batch target-batch-id tx-sender))
            (map-set BatchDetails { batch-id: target-batch-id } {
                producer: (get producer first-batch),
                harvest-date: (get harvest-date first-batch),
                harvest-location: (get harvest-location first-batch),
                produce-type: (get produce-type first-batch),
                quantity: total-quantity,
                current-status: "merged",
                current-holder: tx-sender,
            })
            (map-set BatchMergeHistory { merge-id: merge-id } {
                operator: tx-sender,
                merged-at: stacks-block-height,
                source-batches: source-batch-ids,
                target-batch-id: target-batch-id,
                total-quantity: total-quantity,
                reason: reason,
            })
            (var-set last-batch-id target-batch-id)
            (var-set last-merge-id merge-id)
            (ok target-batch-id)
        )
    )
)

(define-private (validate-merge-compatibility
        (batch-ids (list 10 uint))
        (reference-batch { producer: principal, harvest-date: uint, harvest-location: (string-ascii 50), produce-type: (string-ascii 30), quantity: uint, current-status: (string-ascii 20), current-holder: principal })
    )
    (fold validate-single-batch batch-ids true)
)

(define-private (validate-single-batch (batch-id uint) (valid bool))
    (if (not valid)
        false
        (let ((batch-data (map-get? BatchDetails { batch-id: batch-id })))
            (match batch-data
                some-batch (and
                    (is-eq tx-sender (get current-holder some-batch))
                    (not (is-eq (get current-status some-batch) "recalled"))
                )
                false
            )
        )
    )
)

(define-private (calculate-total-quantity (batch-ids (list 10 uint)))
    (fold sum-batch-quantity batch-ids u0)
)

(define-private (sum-batch-quantity (batch-id uint) (total uint))
    (let ((batch-data (map-get? BatchDetails { batch-id: batch-id })))
        (match batch-data
            some-batch (+ total (get quantity some-batch))
            total
        )
    )
)

(define-private (burn-source-batches (batch-ids (list 10 uint)))
    (fold burn-single-batch batch-ids (ok true))
)

(define-private (burn-single-batch (batch-id uint) (result (response bool uint)))
    (match result
        ok-val (nft-burn? produce-batch batch-id tx-sender)
        err-val (err err-val)
    )
)

(define-read-only (get-split-history
        (parent-batch-id uint)
        (split-id uint)
    )
    (ok (unwrap!
        (map-get? BatchSplitHistory {
            parent-batch-id: parent-batch-id,
            split-id: split-id,
        })
        err-batch-not-found
    ))
)

(define-read-only (get-merge-history (merge-id uint))
    (ok (unwrap!
        (map-get? BatchMergeHistory { merge-id: merge-id })
        err-batch-not-found
    ))
)

(define-map ExpirationMetadata
    { batch-id: uint }
    {
        expiration-block: uint,
        shelf-life-days: uint,
        storage-conditions: (string-ascii 50),
        is-perishable: bool,
        freshness-alerts: (list 5 uint),
    }
)

(define-map FreshnessCheckpoints
    {
        batch-id: uint,
        checkpoint-id: uint,
    }
    {
        checked-at: uint,
        checker: principal,
        freshness-score: uint,
        notes: (string-ascii 100),
    }
)

(define-data-var last-checkpoint-id uint u0)

(define-public (set-expiration
        (batch-id uint)
        (shelf-life-blocks uint)
        (storage-conditions (string-ascii 50))
        (is-perishable bool)
    )
    (let ((batch-data (unwrap! (map-get? BatchDetails { batch-id: batch-id })
            err-batch-not-found
        )))
        (asserts! (is-eq tx-sender (get current-holder batch-data))
            err-not-authorized
        )
        (asserts! (is-none (map-get? ExpirationMetadata { batch-id: batch-id }))
            err-expiration-exists
        )
        (map-set ExpirationMetadata { batch-id: batch-id } {
            expiration-block: (+ stacks-block-height shelf-life-blocks),
            shelf-life-days: shelf-life-blocks,
            storage-conditions: storage-conditions,
            is-perishable: is-perishable,
            freshness-alerts: (list),
        })
        (ok true)
    )
)

(define-public (record-freshness-check
        (batch-id uint)
        (freshness-score uint)
        (notes (string-ascii 100))
    )
    (let (
            (checkpoint-id (+ (var-get last-checkpoint-id) u1))
            (expiration-data (unwrap! (map-get? ExpirationMetadata { batch-id: batch-id })
                err-batch-not-found
            ))
        )
        (asserts! (<= freshness-score u100) err-invalid-status)
        (map-set FreshnessCheckpoints {
            batch-id: batch-id,
            checkpoint-id: checkpoint-id,
        } {
            checked-at: stacks-block-height,
            checker: tx-sender,
            freshness-score: freshness-score,
            notes: notes,
        })
        (if (and (get is-perishable expiration-data) (< freshness-score u60))
            (let ((updated-alerts (unwrap-panic (as-max-len? (append (get freshness-alerts expiration-data) checkpoint-id) u5))))
                (map-set ExpirationMetadata { batch-id: batch-id }
                    (merge expiration-data { freshness-alerts: updated-alerts })
                )
                (var-set last-checkpoint-id checkpoint-id)
                (ok checkpoint-id)
            )
            (begin
                (var-set last-checkpoint-id checkpoint-id)
                (ok checkpoint-id)
            )
        )
    )
)

(define-public (mark-batch-expired (batch-id uint))
    (let (
            (batch-data (unwrap! (map-get? BatchDetails { batch-id: batch-id })
                err-batch-not-found
            ))
            (expiration-data (unwrap! (map-get? ExpirationMetadata { batch-id: batch-id })
                err-batch-not-found
            ))
        )
        (asserts!
            (or 
                (is-eq tx-sender (get current-holder batch-data))
                (>= stacks-block-height (get expiration-block expiration-data))
            )
            err-not-authorized
        )
        (map-set BatchDetails { batch-id: batch-id }
            (merge batch-data { current-status: "expired" })
        )
        (ok true)
    )
)

(define-read-only (get-expiration-metadata (batch-id uint))
    (ok (unwrap!
        (map-get? ExpirationMetadata { batch-id: batch-id })
        err-batch-not-found
    ))
)

(define-read-only (is-batch-expired (batch-id uint))
    (let ((expiration-data (unwrap! (map-get? ExpirationMetadata { batch-id: batch-id })
            err-batch-not-found
        )))
        (ok (>= stacks-block-height (get expiration-block expiration-data)))
    )
)

(define-read-only (get-remaining-shelf-life (batch-id uint))
    (let ((expiration-data (unwrap! (map-get? ExpirationMetadata { batch-id: batch-id })
            err-batch-not-found
        )))
        (if (>= stacks-block-height (get expiration-block expiration-data))
            (ok u0)
            (ok (- (get expiration-block expiration-data) stacks-block-height))
        )
    )
)

(define-read-only (get-freshness-checkpoint
        (batch-id uint)
        (checkpoint-id uint)
    )
    (ok (unwrap!
        (map-get? FreshnessCheckpoints {
            batch-id: batch-id,
            checkpoint-id: checkpoint-id,
        })
        err-batch-not-found
    ))
)

(define-read-only (get-freshness-alerts (batch-id uint))
    (let ((expiration-data (unwrap! (map-get? ExpirationMetadata { batch-id: batch-id })
            err-batch-not-found
        )))
        (ok (get freshness-alerts expiration-data))
    )
)

(define-map BatchAnalytics
    { batch-id: uint }
    {
        total-transfers: uint,
        total-handlers: uint,
        first-transfer-time: uint,
        last-transfer-time: uint,
        average-handling-time: uint,
        quality-checks: uint,
        average-quality-score: uint,
        total-distance-traveled: uint,
        certifications-count: uint,
        recall-count: uint,
    }
)

(define-map HandlerMetrics
    { handler: principal }
    {
        batches-handled: uint,
        total-transfers: uint,
        average-handling-time: uint,
        quality-issues: uint,
        successful-deliveries: uint,
    }
)

(define-map LocationMetrics
    { location: (string-ascii 50) }
    {
        batches-processed: uint,
        average-temperature: uint,
        average-humidity: uint,
        total-visits: uint,
    }
)

(define-public (initialize-analytics (batch-id uint))
    (let ((batch-data (unwrap! (map-get? BatchDetails { batch-id: batch-id })
            err-batch-not-found
        )))
        (asserts! (is-none (map-get? BatchAnalytics { batch-id: batch-id }))
            err-batch-exists
        )
        (map-set BatchAnalytics { batch-id: batch-id } {
            total-transfers: u0,
            total-handlers: u1,
            first-transfer-time: u0,
            last-transfer-time: u0,
            average-handling-time: u0,
            quality-checks: u0,
            average-quality-score: u100,
            total-distance-traveled: u0,
            certifications-count: u0,
            recall-count: u0,
        })
        (ok true)
    )
)

(define-public (update-transfer-analytics
        (batch-id uint)
        (handling-time uint)
    )
    (let (
            (analytics (unwrap! (map-get? BatchAnalytics { batch-id: batch-id })
                err-batch-not-found
            ))
            (new-total-transfers (+ (get total-transfers analytics) u1))
            (current-time stacks-block-height)
            (new-avg-handling-time (/ (+ (* (get average-handling-time analytics) (get total-transfers analytics)) handling-time) new-total-transfers))
        )
        (map-set BatchAnalytics { batch-id: batch-id }
            (merge analytics {
                total-transfers: new-total-transfers,
                last-transfer-time: current-time,
                average-handling-time: new-avg-handling-time,
                first-transfer-time: (if (is-eq (get first-transfer-time analytics) u0) current-time (get first-transfer-time analytics)),
            })
        )
        (update-handler-metrics tx-sender handling-time)
    )
)

(define-public (update-quality-analytics
        (batch-id uint)
        (quality-score uint)
    )
    (let (
            (analytics (unwrap! (map-get? BatchAnalytics { batch-id: batch-id })
                err-batch-not-found
            ))
            (new-quality-checks (+ (get quality-checks analytics) u1))
            (new-avg-quality (/ (+ (* (get average-quality-score analytics) (get quality-checks analytics)) quality-score) new-quality-checks))
        )
        (asserts! (<= quality-score u100) err-invalid-status)
        (map-set BatchAnalytics { batch-id: batch-id }
            (merge analytics {
                quality-checks: new-quality-checks,
                average-quality-score: new-avg-quality,
            })
        )
        (ok true)
    )
)

(define-public (increment-certification-count (batch-id uint))
    (let ((analytics (unwrap! (map-get? BatchAnalytics { batch-id: batch-id })
            err-batch-not-found
        )))
        (map-set BatchAnalytics { batch-id: batch-id }
            (merge analytics {
                certifications-count: (+ (get certifications-count analytics) u1),
            })
        )
        (ok true)
    )
)

(define-public (increment-recall-count (batch-id uint))
    (let ((analytics (unwrap! (map-get? BatchAnalytics { batch-id: batch-id })
            err-batch-not-found
        )))
        (map-set BatchAnalytics { batch-id: batch-id }
            (merge analytics {
                recall-count: (+ (get recall-count analytics) u1),
            })
        )
        (ok true)
    )
)

(define-private (update-handler-metrics
        (handler principal)
        (handling-time uint)
    )
    (let ((metrics-opt (map-get? HandlerMetrics { handler: handler })))
        (match metrics-opt
            existing-metrics
                (let (
                        (new-total-transfers (+ (get total-transfers existing-metrics) u1))
                        (new-avg-handling (/ (+ (* (get average-handling-time existing-metrics) (get total-transfers existing-metrics)) handling-time) new-total-transfers))
                    )
                    (map-set HandlerMetrics { handler: handler }
                        (merge existing-metrics {
                            total-transfers: new-total-transfers,
                            average-handling-time: new-avg-handling,
                        })
                    )
                    (ok true)
                )
            (begin
                (map-set HandlerMetrics { handler: handler } {
                    batches-handled: u1,
                    total-transfers: u1,
                    average-handling-time: handling-time,
                    quality-issues: u0,
                    successful-deliveries: u0,
                })
                (ok true)
            )
        )
    )
)

(define-public (update-location-metrics
        (location (string-ascii 50))
        (temperature uint)
        (humidity uint)
    )
    (let ((metrics-opt (map-get? LocationMetrics { location: location })))
        (match metrics-opt
            existing-metrics
                (let (
                        (new-total-visits (+ (get total-visits existing-metrics) u1))
                        (new-avg-temp (/ (+ (* (get average-temperature existing-metrics) (get total-visits existing-metrics)) temperature) new-total-visits))
                        (new-avg-humidity (/ (+ (* (get average-humidity existing-metrics) (get total-visits existing-metrics)) humidity) new-total-visits))
                    )
                    (map-set LocationMetrics { location: location } {
                        batches-processed: (get batches-processed existing-metrics),
                        average-temperature: new-avg-temp,
                        average-humidity: new-avg-humidity,
                        total-visits: new-total-visits,
                    })
                    (ok true)
                )
            (begin
                (map-set LocationMetrics { location: location } {
                    batches-processed: u1,
                    average-temperature: temperature,
                    average-humidity: humidity,
                    total-visits: u1,
                })
                (ok true)
            )
        )
    )
)

(define-public (increment-handler-deliveries (handler principal))
    (let ((metrics (unwrap! (map-get? HandlerMetrics { handler: handler })
            err-batch-not-found
        )))
        (map-set HandlerMetrics { handler: handler }
            (merge metrics {
                successful-deliveries: (+ (get successful-deliveries metrics) u1),
            })
        )
        (ok true)
    )
)

(define-public (record-quality-issue (handler principal))
    (let ((metrics (unwrap! (map-get? HandlerMetrics { handler: handler })
            err-batch-not-found
        )))
        (map-set HandlerMetrics { handler: handler }
            (merge metrics {
                quality-issues: (+ (get quality-issues metrics) u1),
            })
        )
        (ok true)
    )
)

(define-read-only (get-batch-analytics (batch-id uint))
    (ok (unwrap!
        (map-get? BatchAnalytics { batch-id: batch-id })
        err-batch-not-found
    ))
)

(define-read-only (get-handler-metrics (handler principal))
    (ok (unwrap!
        (map-get? HandlerMetrics { handler: handler })
        err-batch-not-found
    ))
)

(define-read-only (get-location-metrics (location (string-ascii 50)))
    (ok (unwrap!
        (map-get? LocationMetrics { location: location })
        err-batch-not-found
    ))
)

(define-read-only (get-batch-efficiency-score (batch-id uint))
    (let ((analytics (unwrap! (map-get? BatchAnalytics { batch-id: batch-id })
            err-batch-not-found
        )))
        (let (
                (quality-weight (* (get average-quality-score analytics) u4))
                (transfer-penalty (* (get total-transfers analytics) u2))
                (recall-penalty (* (get recall-count analytics) u50))
                (raw-score (if (> (+ quality-weight u100) (+ transfer-penalty recall-penalty))
                    (- (+ quality-weight u100) (+ transfer-penalty recall-penalty))
                    u0
                ))
            )
            (ok (if (> raw-score u100) u100 raw-score))
        )
    )
)

(define-read-only (get-handler-reliability-score (handler principal))
    (let ((metrics (unwrap! (map-get? HandlerMetrics { handler: handler })
            err-batch-not-found
        )))
        (let (
                (success-rate (if (> (get total-transfers metrics) u0)
                    (/ (* (get successful-deliveries metrics) u100) (get total-transfers metrics))
                    u0
                ))
                (issue-penalty (* (get quality-issues metrics) u5))
                (reliability (if (> success-rate issue-penalty)
                    (- success-rate issue-penalty)
                    u0
                ))
            )
            (ok (if (> reliability u100) u100 reliability))
        )
    )
)
