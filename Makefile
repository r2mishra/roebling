
single:
	uvicorn dummy_server.app:app --workers 1 --port 8000

multi:
	uvicorn dummy_server.app:app --workers 4

mega:
	uvicorn dummy_server.app:app --workers 40 --port 8001

run:
	stack run http://localhost:8000/slow

dev:
	hpack
	cabal build
	cabal run roebling-exe  -- --method GET http://localhost:8000/slow --rate 20 --duration 30

fmt:
	find . -name '*.hs' | xargs ormolu --mode inplace

runtest:
	cabal test --test-show-details=always

mega_demo:
	cabal run roebling-exe  -- --method GET http://localhost:8001/slow --rate 20 --duration 15

single_demo:
	cabal run roebling-exe  -- --method GET http://localhost:8000/slow --rate 20 --duration 15

post_demo:
	cabal run roebling-exe  -- --method POST http://localhost:8001/post --body-file dummy_server/dummy_payload.json --rate 20 --duration 15
