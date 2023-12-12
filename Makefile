
single:
	uvicorn dummy_server.app:app --workers 1

multi:
	uvicorn dummy_server.app:app --workers 4

mega:
	uvicorn dummy_server.app:app --workers 40

run:
	stack run http://localhost:8000/slow

dev:
	hpack
	cabal build
	cabal run roebling-exe  -- --method POST http://localhost:8000/post --body-file dummy_server/dummy_payload.json --rate 10 --duration 5 --plotDemo

fmt:
	find . -name '*.hs' | xargs ormolu --mode inplace