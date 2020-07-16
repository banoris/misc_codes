from multiprocessing import Process
from time import sleep
import os

# Grokking multiprocessing in umbra/scenario/main.py
class Forever():
    def __init__(self):
        self.init()

    def init(self):
        self.loop()

    def loop(self):
        print("FOREVER: loop START")
        sleep_counter = 0;
        while True:
            if (sleep_counter == 5):
                break
            sleep(1)
            print(f'   FOREVER: sleep_counter={sleep_counter}')
            sleep_counter += 1

        print("FOREVER: loop END")
        

def info(title):
    print("===================================")
    print(title)
    print('module name:', __name__)
    print('parent process:', os.getppid())
    print('process id:', os.getpid())
    print("===================================")

def f(name):
    info('function f')
    print('hello', name)

def init(input1, input2):
    print(input1, input2)
    print("Starting Forever class")
    Forever()
    print("Done init")


if __name__ == '__main__':
    info('MAIN: START')
    p = Process(target=init, args=('bello', 'bello2'))
    p.start()
    for i in range(10):
        print(f'MAIN: sleep count={i}')
        sleep(0.5)
    # NOTE: no, not magic. join() will wait until init() above is
    # done. init() is blocked by Forever object. Inside Forever's __init__,
    # it will be blocked at a while true loop
    p.join()
    print("MAIN: END")


# ===============
# Output analysis
# ===============
"""
From below, output from main process (MAIN) and the spawned process(FOREVER)
interleaved. Meaning the main process and the spawned process are not blocked on
each other. When the Forever process is launched (via Process.start()), a new
execution starts


$ python3.7 multiprocessing1.py
===================================
MAIN: START
module name: __main__
parent process: 111049
process id: 63614
===================================
MAIN: sleep count=0
bello bello2
Starting Forever class
FOREVER: loop START
MAIN: sleep count=1
MAIN: sleep count=2
   FOREVER: sleep_counter=0
MAIN: sleep count=3
   FOREVER: sleep_counter=1
MAIN: sleep count=4
MAIN: sleep count=5
   FOREVER: sleep_counter=2
MAIN: sleep count=6
MAIN: sleep count=7
   FOREVER: sleep_counter=3
MAIN: sleep count=8
MAIN: sleep count=9
   FOREVER: sleep_counter=4
FOREVER: loop END
Done init
MAIN: END

"""
