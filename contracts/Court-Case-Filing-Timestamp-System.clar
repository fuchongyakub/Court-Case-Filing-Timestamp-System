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
        (if (is-eq current-count u0)
            (begin
                (unwrap-panic (update-search-indices case-id jurisdiction case-type status-pending))
                (ok current-time))
            (ok current-time))))
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

(define-constant status-pending "pending")
(define-constant status-active "active") 
(define-constant status-closed "closed")
(define-constant status-appealed "appealed")
(define-constant err-invalid-status (err u106))

(define-map case-status-history
    { case-id: (string-ascii 32), status-sequence: uint }
    {
        status: (string-ascii 16),
        updated-by: principal,
        timestamp: uint,
        notes: (string-ascii 256)
    }
)

(define-map case-status-index
    { case-id: (string-ascii 32) }
    { current-status: (string-ascii 16), status-count: uint }
)

(define-private (is-valid-status (status (string-ascii 16)))
    (or (is-eq status status-pending)
        (or (is-eq status status-active)
            (or (is-eq status status-closed)
                (is-eq status status-appealed)))))

(define-public (update-case-status 
    (case-id (string-ascii 32))
    (new-status (string-ascii 16))
    (notes (string-ascii 256)))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (status-info (default-to {current-status: status-pending, status-count: u0} 
                                 (map-get? case-status-index {case-id: case-id})))
         (current-count (get status-count status-info)))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (is-valid-status new-status) err-invalid-status)
        (asserts! (> (get-case-count case-id) u0) err-case-not-found)
        (map-set case-status-history
            {case-id: case-id, status-sequence: current-count}
            {status: new-status, updated-by: caller, timestamp: current-time, notes: notes})
        (map-set case-status-index
            {case-id: case-id}
            {current-status: new-status, status-count: (+ current-count u1)})
        (ok current-time)))

(define-read-only (get-current-case-status (case-id (string-ascii 32)))
    (get current-status (default-to {current-status: status-pending, status-count: u0}
                                   (map-get? case-status-index {case-id: case-id}))))

(define-read-only (get-case-status-history (case-id (string-ascii 32)) (status-sequence uint))
    (map-get? case-status-history {case-id: case-id, status-sequence: status-sequence}))

(define-read-only (get-case-status-count (case-id (string-ascii 32)))
    (get status-count (default-to {current-status: status-pending, status-count: u0}
                                 (map-get? case-status-index {case-id: case-id}))))

                                 (define-constant err-doc-not-found (err u107))
                                 
(define-constant err-already-verified (err u108))
(define-constant err-verification-failed (err u109))

(define-map document-metadata
    { doc-hash: (buff 32) }
    {
        case-id: (string-ascii 32),
        doc-type: (string-ascii 32),
        file-size: uint,
        uploader: principal,
        upload-timestamp: uint,
        is-verified: bool,
        verification-timestamp: (optional uint),
        verified-by: (optional principal)
    }
)

(define-map case-documents
    { case-id: (string-ascii 32), doc-index: uint }
    { doc-hash: (buff 32) }
)

(define-map case-doc-count
    { case-id: (string-ascii 32) }
    { count: uint }
)

(define-public (register-document
    (case-id (string-ascii 32))
    (document-hash (buff 32))
    (doc-type (string-ascii 32))
    (file-size uint))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (doc-count (default-to u0 (get count (map-get? case-doc-count {case-id: case-id})))))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (> (len document-hash) u0) err-invalid-hash)
        (asserts! (is-none (map-get? document-metadata {doc-hash: document-hash})) err-duplicate)
        (map-set document-metadata
            {doc-hash: document-hash}
            {case-id: case-id, doc-type: doc-type, file-size: file-size,
             uploader: caller, upload-timestamp: current-time, is-verified: false,
             verification-timestamp: none, verified-by: none})
        (map-set case-documents
            {case-id: case-id, doc-index: doc-count}
            {doc-hash: document-hash})
        (map-set case-doc-count
            {case-id: case-id}
            {count: (+ doc-count u1)})
        (ok current-time)))

(define-public (verify-document (document-hash (buff 32)))
    (let
        ((caller tx-sender)
         (current-time stacks-block-height)
         (doc-info (unwrap! (map-get? document-metadata {doc-hash: document-hash}) err-doc-not-found)))
        (asserts! (is-entity-authorized caller) err-not-registered)
        (asserts! (not (get is-verified doc-info)) err-already-verified)
        (map-set document-metadata
            {doc-hash: document-hash}
            (merge doc-info {is-verified: true, 
                           verification-timestamp: (some current-time),
                           verified-by: (some caller)}))
        (ok current-time)))

(define-read-only (get-document-info (document-hash (buff 32)))
    (map-get? document-metadata {doc-hash: document-hash}))

(define-read-only (get-case-document (case-id (string-ascii 32)) (doc-index uint))
    (map-get? case-documents {case-id: case-id, doc-index: doc-index}))

(define-read-only (get-case-document-count (case-id (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? case-doc-count {case-id: case-id}))))

(define-read-only (is-document-verified (document-hash (buff 32)))
    (match (map-get? document-metadata {doc-hash: document-hash})
        doc-info (get is-verified doc-info)
        false))

(define-constant err-invalid-search (err u110))
(define-constant max-search-results u50)

(define-map jurisdiction-cases
    { jurisdiction: (string-ascii 32), jurisdiction-index: uint }
    { case-id: (string-ascii 32) }
)

(define-map jurisdiction-index
    { jurisdiction: (string-ascii 32) }
    { count: uint }
)

(define-map case-type-cases
    { case-type: (string-ascii 32), type-index: uint }
    { case-id: (string-ascii 32) }
)

(define-map case-type-index
    { case-type: (string-ascii 32) }
    { count: uint }
)

(define-map status-cases
    { status: (string-ascii 16), status-index: uint }
    { case-id: (string-ascii 32) }
)

(define-map status-search-index
    { status: (string-ascii 16) }
    { count: uint }
)

(define-private (update-search-indices (case-id (string-ascii 32)) (jurisdiction (string-ascii 32)) (case-type (string-ascii 32)) (status (string-ascii 16)))
    (let
        ((jurisdiction-count (default-to u0 (get count (map-get? jurisdiction-index {jurisdiction: jurisdiction}))))
         (type-count (default-to u0 (get count (map-get? case-type-index {case-type: case-type}))))
         (status-count (default-to u0 (get count (map-get? status-search-index {status: status})))))
        (map-set jurisdiction-cases
            {jurisdiction: jurisdiction, jurisdiction-index: jurisdiction-count}
            {case-id: case-id})
        (map-set jurisdiction-index
            {jurisdiction: jurisdiction}
            {count: (+ jurisdiction-count u1)})
        (map-set case-type-cases
            {case-type: case-type, type-index: type-count}
            {case-id: case-id})
        (map-set case-type-index
            {case-type: case-type}
            {count: (+ type-count u1)})
        (map-set status-cases
            {status: status, status-index: status-count}
            {case-id: case-id})
        (map-set status-search-index
            {status: status}
            {count: (+ status-count u1)})
        (ok true)))

(define-read-only (search-by-jurisdiction (jurisdiction (string-ascii 32)) (limit uint) (offset uint))
    (let
        ((total-count (default-to u0 (get count (map-get? jurisdiction-index {jurisdiction: jurisdiction}))))
         (search-limit (if (> limit max-search-results) max-search-results limit)))
        (asserts! (< offset total-count) err-invalid-search)
        (ok {
            total: total-count,
            limit: search-limit,
            offset: offset
        })))

(define-read-only (get-jurisdiction-case (jurisdiction (string-ascii 32)) (index uint))
    (map-get? jurisdiction-cases {jurisdiction: jurisdiction, jurisdiction-index: index}))

(define-read-only (search-by-case-type (case-type (string-ascii 32)) (limit uint) (offset uint))
    (let
        ((total-count (default-to u0 (get count (map-get? case-type-index {case-type: case-type}))))
         (search-limit (if (> limit max-search-results) max-search-results limit)))
        (asserts! (< offset total-count) err-invalid-search)
        (ok {
            total: total-count,
            limit: search-limit,
            offset: offset
        })))

(define-read-only (get-case-type-case (case-type (string-ascii 32)) (index uint))
    (map-get? case-type-cases {case-type: case-type, type-index: index}))

(define-read-only (search-by-status (status (string-ascii 16)) (limit uint) (offset uint))
    (let
        ((total-count (default-to u0 (get count (map-get? status-search-index {status: status}))))
         (search-limit (if (> limit max-search-results) max-search-results limit)))
        (asserts! (< offset total-count) err-invalid-search)
        (ok {
            total: total-count,
            limit: search-limit,
            offset: offset
        })))

(define-read-only (get-status-case (status (string-ascii 16)) (index uint))
    (map-get? status-cases {status: status, status-index: index}))

(define-read-only (get-jurisdiction-count (jurisdiction (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? jurisdiction-index {jurisdiction: jurisdiction}))))

(define-read-only (get-case-type-count (case-type (string-ascii 32)))
    (get count (default-to {count: u0} (map-get? case-type-index {case-type: case-type}))))

(define-read-only (get-status-count (status (string-ascii 16)))
    (get count (default-to {count: u0} (map-get? status-search-index {status: status}))))