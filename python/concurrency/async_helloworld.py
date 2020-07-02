import asyncio

async def printhello(input):
    print('Hello', input);
    await asyncio.sleep(1)
    print('....World', input);

async def main(input=None):
    print('Hello', input)
    await asyncio.sleep(1)
    print('... World!')
    await asyncio.sleep(1)
    print('... World2!')

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
    await asyncio.gather(printhello("1"), printhello("2"))

asyncio.run(main2())

# SyntaxError: 'await' outside async function
#def top_func():
#    await main("1")

# Python 3.7+
#asyncio.run(main("1"))
# await asyncio.gather(main("1"), main("2")) # compile error

# syntax error: await outside function
# await only allowed inside function. Why?
#await main("1")


