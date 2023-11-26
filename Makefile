
single:
	uvicorn dummy_server.app:app --workers 1

multi:
	uvicorn dummy_server.app:app --workers 4

mega:
	uvicorn dummy_server.app:app --workers 40

run:
	stack run http://localhost:8000/slow

dev:
	stack build
	stack install
	roebling-exe --method GET http://localhost:8000/slow --duration 10

fmt:
	find . -name '*.hs' | xargs ormolu --mode inplace