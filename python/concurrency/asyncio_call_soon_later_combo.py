import asyncio
import datetime

# display datetime.now() every second for 5 seconds
def display_date(end_time, loop):
    print(datetime.datetime.now())
    if (loop.time() + 1.0) < end_time:
        loop.call_later(1, display_date, end_time, loop)
    else:
        loop.stop()

loop = asyncio.get_event_loop()

# Schedule the first call to display_date()
# call_soon(callback, *args, context=None) will schedule the callback
# at the next iteration of the event_loop. So, it means that at the
# next timeslice, execute the given callback
end_time = loop.time() + 5.0
loop.call_soon(display_date, end_time, loop)

# Blocking call interrupted by loop.stop()
try:
    loop.run_forever()
finally:
    loop.close()


###########################
# python 3.8 using run()
###########################

# Why we don't need loop.close()?
# asyncio.run() takes care of that for you

async def display_date():
    loop = asyncio.get_running_loop()
    end_time = loop.time() + 5.0
    while True:
        print(datetime.datetime.now())
        if (loop.time() + 1.0) >= end_time:
            break
        await asyncio.sleep(1)

# asyncio.run(display_date())
