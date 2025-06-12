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
