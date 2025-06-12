# chronobid

**chronobid** is a minimal order matching engine written in Haskell.

## features

- simple in-memory order book
- supports market and limit orders
- JSON API for order submission and book snapshot
- static frontend to view the order book
- pure + STM-based core logic

## running
```bash
cabal run server
```

## deploying
```
terraform apply
echo "<vps ip address>" > nixaws
bash deploy.sh
```

## in progress
* fix market order matching
* endpoint for returning completed trades


## sample usage
```
> # sumbit order
> curl  -X POST https://nocoverletter.idrisraja.com/order \
  -H "Content-Type: application/json" \
  -d '{"orderId":"00000000-0000-0000-0000-000000000000","timestamp":"2024-06-10T12:00:30Z","side":"Sell","orderType":"Limit","price":69.4,"quantity":400.0,"userId":"00000000-0000-0000-0000-000000000000"}'
  ```

```
> # get order book
> curl  https://nocoverletter.idrisraja.com/orderbook
```
