import asyncio

async def run(cmd):
    proc = await asyncio.create_subprocess_shell(
        cmd,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE)

    # Why need this subprocess.communicate?
    # Well, we are in the async world - if you don't block here
    # and wait for the child to return, then we won't get any output
    stdout, stderr = await proc.communicate()

    print(f'[{cmd!r} exited with {proc.returncode}]')
    if stdout:
        print(f'[stdout]\n{stdout.decode()}')
    if stderr:
        print(f'[stderr]\n{stderr.decode()}')

#asyncio.run(run('ls /asdzxc'))
#asyncio.run(run('ls $PWD'))


async def main():
    await asyncio.gather(
            run('ls -la'),
            run('sleep 1; echo "hello"')
        )

print("===== execute multiple coro concurrently =====")
asyncio.run(main())
