import asyncio
import time

# https://docs.python.org/3.8/library/asyncio-task.html?highlight=create_task#coroutines
# To actually run a coroutine, asyncio provides three main mechanisms

async def say_after(delay, what):
    await asyncio.sleep(delay)
    print(what)

async def get_val_after(delay, value):
    await asyncio.sleep(delay)
    return value

async def main_not_concurrent():
    print(f"started at {time.strftime('%X')}")

    await say_after(1, 'hello')
    await say_after(2, 'world')

    print(f"finished at {time.strftime('%X')}")

# NOTE: weird... not expecting it to take 2secs from reading this guy's blog
# https://whatisjasongoldstein.com/writing/im-too-stupid-for-asyncio
def sync_all():
    print(f"started at {time.strftime('%X')}")

    loop = asyncio.get_event_loop()
    tasks = [say_after(1, 'hello'), say_after(2, 'world')]
    loop.run_until_complete(asyncio.gather(*tasks))
    loop.close()
    print(f"finished at {time.strftime('%X')}")

async def main_concurrent():
    task1 = asyncio.create_task(
        say_after(1, 'hello'))

    task2 = asyncio.create_task(
        say_after(2, 'world'))

    print(f"started at {time.strftime('%X')}")

    # Wait until both tasks are completed (should take
    # around 2 seconds.)
    # NOTE: what happen if you don't have await here? Does task1 and task2 run?
    #   Nope, this main_concurrent just terminates
    # NOTE: What happen if you put asyncio.sleep instead of await task1? Tricky question...
    #   create_task *schedule* the coro to run, but the coro won't run if this main doesn't
    #   preempt itself. Remember, in cooperative scheduling, you yourself are responsible
    #   to give up your cpu timeslice so that others can use the cpu. E.g., you block on
    #   something, then the EventLoop takes away the cpu from you and give it to other coro
    await task1
    await task2
    # What happen when you await the same Task Object? Does it run again?
    # I.e., does 'hello' printed twice? NOPE!
    await task1
    if task1.done():
        print("task1 done")

    # Does this concurrent as well? YES! Still takes 2 secs
    # tasks = [task1, task2]
    # await asyncio.gather(*tasks)

    print(f"finished at {time.strftime('%X')}")


# NOTE: refer example 2 from this guy blog
# https://whatisjasongoldstein.com/writing/im-too-stupid-for-asyncio
# He used run_until_complete and said it's not concurrent. Does using asyncio.run()
# differs?
async def main_concurrent2():
    print(f"started at {time.strftime('%X')}")

    tasks = [say_after(1, 'hello'), say_after(2, 'world')]

    await asyncio.gather(*tasks)

    print(f"finished at {time.strftime('%X')}")


async def main_concurrent_with_ret():
    task1 = asyncio.create_task(
        get_val_after(1, 'hello'))

    task2 = asyncio.create_task(
            get_val_after(2, {'text': "world"}))

    print(f"started at {time.strftime('%X')}")

    # Does this concurrent as well? YES! Still takes 2 secs
    tasks = [task1, task2]
    print("task1 = ", task1)
    ret1, ret2 = await asyncio.gather(*tasks)
    print("retval =", ret1, ret2)

    print(f"finished at {time.strftime('%X')}")

# Is below output concurrent? Took 3 secs total. No! During sleep, another coro
# should run. So, we are expecting to finish in 2 seconds
#       started at 16:58:36
#       hello
#       world
#       finished at 16:58:39
#asyncio.run(main_not_concurrent())

# NOTE: why is this still takes 2 seconds?
# This guy https://whatisjasongoldstein.com/writing/im-too-stupid-for-asyncio said
# it is not concurrent?
#sync_all()

# Using Task Object. From output below, is it concurrent? Yes!
# Only takes 2secs to complete
#     started at 17:03:28
#     hello
#     world
#     finished at 17:03:30
# Tl;dr -- await <TaskObject> is concurrent
#       -- await <coroutine>  is NOT concurrent
#asyncio.run(main_concurrent())



# NOTE: another example with return value
asyncio.run(main_concurrent_with_ret())

