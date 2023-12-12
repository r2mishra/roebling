
from fastapi import FastAPI, Request
import json
import time

app = FastAPI()


@app.get("/ping")
async def ping():
    start_time = time.time()
    result = {"result": "pong"}
    end_time = time.time()
    latency = end_time - start_time
    print(f"Latency for /ping: {latency}")
    return result

@app.get("/slow")
async def slow():
    start_time = time.time()
    time.sleep(0.2)
    result = {"result": "pong"}
    end_time = time.time()
    latency = end_time - start_time
    print(f"Latency for /slow: {latency}")
    return result

@app.post("/post")
async def post(request: Request):
    start_time = time.time()
    data = await request.json()
    print(data)
    result = json.dumps(data)
    end_time = time.time()
    latency = end_time - start_time
    print(f"Latency for /post: {latency}")
    return result


