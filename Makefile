
single:
	uvicorn dummy_server.app:app --workers 1

multi:
	uvicorn dummy_server.app:app --workers 4

mega:
	uvicorn dummy_server.app:app --workers 40

run:
	stack run http://localhost:8000/slow
