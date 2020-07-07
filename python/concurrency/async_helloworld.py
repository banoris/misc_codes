import asyncio

async def printhello(input):
    print('Hello', input);
    await asyncio.sleep(1)
    print('....World', input);

async def main(input=None):
    print('Hello', input)
    await asyncio.sleep(1)
    print('... World', input)
    await asyncio.sleep(1)
    print('... World2', input)

async def main2(input=None):
    # looks asynchronous but it's actually not
    # Why? Because we are expecting both 'Hello1' and 'Hello2' to be printed
    # and not blocked by the sleep
    #await printhello("1")
    #await printhello("2")

    # true async - should observe both 'Hello from' printed

    # Why below exception? Because you forget to await!!
    # NOTE: asyncio.gather returns awaitable which you need to await
    # _GatheringFuture exception was never retrieved
    # future: <_GatheringFuture finished exception=CancelledError()>
    # concurrent.futures._base.CancelledError
    await asyncio.gather(printhello(input), printhello(input))

async def main3(input=None):
    aws = [main2("coro1"), main2("coro2"), main2("coro3")]
    # NOTE: don't forget 'await' keyword in front of gather!!!
    # https://docs.python.org/3/library/asyncio-api-index.html
    #   .run():    create EventLoop, run the passed coroutine, and close loop
    #   .gather(): schedule coroutine(s) concurrently and wait for them to finish
    await asyncio.gather(*aws)
    #print("done main3", aws)

asyncio.run(main3())
print("done!")

# SyntaxError: 'await' outside async function
#def top_func():
#    await main("1")

# Python 3.7+
#asyncio.run(main("1"))
# await asyncio.gather(main("1"), main("2")) # compile error

# syntax error: await outside function
# await only allowed inside function. Why?
#await main("1")


#asyncio.run(main2())
