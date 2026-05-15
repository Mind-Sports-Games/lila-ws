# CLAUDE.md

WebSocket server for PlayStrategy (fork of Lichess). Sits between clients and the lila backend:

```
lila <-> redis <-> lila-ws <-> websocket <-> client
```

## Build & Run

```bash
sbt run
```

Default config (in `src/main/resources/application.conf`):
- HTTP port: `9664`
- MongoDB: `mongodb://localhost:27017/lichess`
- Redis: `redis://127.0.0.1`
- CSRF origin: `http://localhost:9663` (lila's port)

Code formatting uses scalafmt — run `scalafmtAll` in the sbt shell before committing.

## Architecture

Each WebSocket connection is managed by a typed Akka actor for its lifetime. `Router` maps URL paths to `Controller` methods, which authenticate via `Auth` (MongoDB session lookup), validate entity existence (MongoDB), then spawn the appropriate actor from `actor/`. `ClientActor` contains shared logic (ping/pong, broom timeout, Bus subscriptions); domain actors (`RoundClientActor`, `StudyClientActor`, etc.) delegate to its companion object methods. `RoundClientActor` is the most complex — it distinguishes player vs watcher and handles move forwarding.

**IPC message types** (`ipc/`) — the naming is directional:
- `ClientOut` / `ClientIn` — browser ↔ lila-ws (JSON)
- `LilaIn` / `LilaOut` — lila-ws ↔ lila via Redis (text protocol)

Redis has one channel pair per domain (site, lobby, round, study, etc.); `round` uses a round-robin across 8 lanes. `Bus` is an in-process pub/sub for fanning `ClientMsg` events to actor subscribers.

**MongoDB** is used only for auth/session lookup and entity existence checks — not for persistent state.

## Sibling Repos

Lives alongside `../lila` (the main Play/Scala backend). See `../lila/CLAUDE.md` for lila's architecture.
