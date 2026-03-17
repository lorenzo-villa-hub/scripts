
import time

def run_every(interval, function, *args, **kwargs):
    while True:
        start = time.time()
        function(*args, **kwargs)        
        elapsed = time.time() - start
        time.sleep(max(0, interval - elapsed))


def test(state):
    print(state["index"])
    print(state["string"])
    state["index"] += 1


state = {"index": 0, "string": "hello"}

run_every(interval=5,function=test,state=state)
