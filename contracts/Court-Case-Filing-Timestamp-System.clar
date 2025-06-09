(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-case-exists (err u101))
(define-constant err-invalid-hash (err u102))
(define-constant err-case-not-found (err u103))
(define-constant err-not-registered (err u104))
(define-constant err-duplicate (err u105))
(define-constant MAX_ENTRIES u100)

(define-data-var system-status bool true)
(define-data-var total-cases uint u0)

(define-map authorized-entities principal bool)
(define-map case-records 
    { case-id: (string-ascii 32) }
    {
        timestamp: uint,
        document-hash: (buff 32),
        filed-by: principal,
        jurisdiction: (string-ascii 32),
        case-type: (string-ascii 32),
        status: (string-ascii 16)
    }
)

(define-map case-history
    { case-id: (string-ascii 32), sequence: uint }
    {
        doc-hash: (buff 32),
        submitter: principal,
        timestamp: uint
    }
)

(define-map case-index
    { case-id: (string-ascii 32) }
    { count: uint }
)

(define-private (check-duplicate (case-id (string-ascii 32)) (doc-hash (buff 32)) (limit uint))
    (let ((existing-entry (map-get? case-history {case-id: case-id, sequence: limit})))
        (if (is-some existing-entry)
            (if (is-eq (get doc-hash (unwrap! existing-entry err-duplicate)) doc-hash)
                err-duplicate
                (ok true))
            (ok true))))

(define-public (file-case 
    (case-id (string-ascii 32))
    (document-hash (buff 32))
    (jurisdiction (string-ascii 32))
    (case-type (string-ascii 32)))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (current-count (default-to u0 (get count (map-get? case-index {case-id: case-id})))))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (< current-count MAX_ENTRIES) err-case-exists)
        (asserts! (> (len document-hash) u0) err-invalid-hash)
        (try! (check-duplicate case-id document-hash current-count))
        (map-set case-history 
            {case-id: case-id, sequence: current-count}
            {doc-hash: document-hash, submitter: caller, timestamp: current-time})
        (map-set case-index 
            {case-id: case-id} 
            {count: (+ current-count u1)})
        (ok current-time)))
(define-public (register-entity (entity principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (ok (map-set authorized-entities entity true))))

(define-public (remove-entity (entity principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (ok (map-delete authorized-entities entity))))

(define-read-only (is-entity-authorized (entity principal))
    (default-to false (map-get? authorized-entities entity)))

(define-read-only (get-case-details (case-id (string-ascii 32)) (sequence uint))
    (map-get? case-history {case-id: case-id, sequence: sequence}))

(define-read-only (get-case-count (case-id (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? case-index {case-id: case-id}))))

(define-read-only (get-total-cases)
    (ok (var-get total-cases)))

(define-public (toggle-system)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (ok (var-set system-status (not (var-get system-status))))))