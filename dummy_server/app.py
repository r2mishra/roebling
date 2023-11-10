
from fastapi import FastAPI
import time
import logging

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


