import logging

# create logger
module_logger = logging.getLogger(__name__) 
module_logger.setLevel(logging.DEBUG)

def some_function():
    module_logger.info('message from submodule')
    module_logger.debug('debug message from submodule')
