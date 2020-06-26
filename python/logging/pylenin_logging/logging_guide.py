# logging_guide.py

import logging

# https://www.pylenin.com/blogs/python-logging-guide

def log_to_file():
    log_format = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    logger = logging.getLogger(__name__)

    # To override the default severity of logging
    logger.setLevel('DEBUG')

    # Use FileHandler() to log to a file
    file_handler = logging.FileHandler("mylogs.log")
    formatter = logging.Formatter(log_format)
    file_handler.setFormatter(formatter)

    # Don't forget to add the file handler
    logger.addHandler(file_handler)
    logger.info("I am a separate Logger")

def log_to_console():
    log_format = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    logger = logging.getLogger(__name__)

    # To override the default severity of logging
    logger.setLevel('DEBUG')

    # Use FileHandler() to log to a file
    file_handler = logging.StreamHandler()
    formatter = logging.Formatter(log_format)
    file_handler.setFormatter(formatter)

    # Don't forget to add the file handler
    logger.addHandler(file_handler)
    logger.info("I am a separate Logger")

log_to_file()
