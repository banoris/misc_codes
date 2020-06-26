import logging

log_format = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
logger = logging.getLogger(__name__)

# To override the default severity of logging
# Default is WARN, so info and debug won't be printed
logger.setLevel('DEBUG')

# Use StreamHandler() to log to console
# file_handler = logging.StreamHandler()
# formatter = logging.Formatter(log_format)
# file_handler.setFormatter(formatter)

# Don't forget to add the file handler
# logger.addHandler(file_handler)
logger.warn("I am a log from helper")
logger.info("I am a log from helper")
logger.debug("I am a log from helper")