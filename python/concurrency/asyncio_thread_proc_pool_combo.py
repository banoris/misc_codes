import asyncio
import concurrent.futures

def blocking_io():
    # File operations (such as logging) can block the
    # event loop: run them in a thread pool.
    with open('/dev/urandom', 'rb') as f:
        return f.read(100)

def cpu_bound():
    # CPU-bound operations will block the event loop:
    # in general it is preferable to run them in a
    # process pool.
    return sum(i * i for i in range(10 ** 7))

async def main_threadpool():
    loop = asyncio.get_running_loop()
    # 2. Run in a custom thread pool:
    with concurrent.futures.ThreadPoolExecutor() as pool:
        result = await loop.run_in_executor(
            pool, blocking_io)
        print('custom thread pool', result)

async def main_procpool():
    loop = asyncio.get_running_loop()

    # 3. Run in a custom process pool:
    with concurrent.futures.ProcessPoolExecutor() as pool:
        # await loop.run_in_executor is blocking
        # Suppose you have 4 cpu cores, how to schedule 4 cpu_bound() work in those 4 cores?
        # https://stackoverflow.com/questions/51117209/combining-asyncio-with-a-multi-worker-processpoolexecutor
        #   Understand how to use asyncio.as_completed(*aws, ...)
        result = await loop.run_in_executor(
            pool, cpu_bound)
        print('custom process pool', result)

asyncio.run(main_procpool())
