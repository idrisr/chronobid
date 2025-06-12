## POST /order

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
{"orderId":"00000000-0000-0000-0000-000000000001","orderType":"Limit","price":100,"quantity":3,"side":"Buy","timestamp":"2025-06-11T18:00:00Z","userId":"00000000-0000-0000-0000-000000000002"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json

```

## GET /orderbook

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```json
{"asks":[],"bids":[]}
```
