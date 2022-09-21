import logging
import traceback
from abc import ABC, abstractmethod
from typing import Callable

log = logging.getLogger(__name__)


# < Custom BioTea errors > #
class BioTeaError(Exception):
    """Generic class from which all BioTea Errors inherit from."""


class InvalidGeoId(BioTeaError):
    """Error raised when an input GEO id is invalid or not found."""

    pass


class UnsupportedChip(BioTeaError):
    """Raised when the chip ID is unsupported."""

    pass


class SanityError(BioTeaError):
    """Raised when a sanity check fails."""

    pass


class ImageNotFoundError(BioTeaError):
    """Raised when remote or local images are not found."""

    pass


class InvalidPathError(BioTeaError):
    """Raised when a given path is invalid."""

    pass


class ContainerExitError(BioTeaError):
    """Raised when the biotea-box exits with an error"""

    pass


# --- #


class ErrorHandler(ABC):
    """Abstract class from which error handlers can be specified."""

    error: Exception = None

    # I am not 100% sure this is the correct way to use this, but it fails
    # if "error" is not defined, so I'm happy.
    def __init_subclass__(cls, /, error, **kwargs):
        super().__init_subclass__(**kwargs)
        cls.error = error

    @abstractmethod
    def handle(error: Exception, invoker: Callable, trace: str, inputs: dict) -> None:
        pass


class ErrorManager:
    """Class to run a fuction and handle errors that it raises.

    Since the calling function is passed to the handle, allows re-running the
    fuction again after the error is handled.
    """

    executable = None

    def __init__(self, executable: Callable) -> None:
        self.handlers = dict()
        self.executable = executable

    def add_handler(self, exception: Exception, handler: ErrorHandler) -> None:
        if exception in self.handlers.keys():
            log.warn(f"Overwriting handler for {exception}.")

        self.handlers.update({exception: handler})

    def run(self, *args, **kwargs):
        try:
            self.executable(*args, **kwargs)
        except Exception as e:
            if e in self.handlers.keys():
                target = e
            elif any(
                (
                    compatible_handlers := [
                        e for x in self.handlers.keys() if issubclass(x, type(e))
                    ]
                )
            ):
                if sum(compatible_handlers) > 1:
                    log.warning(
                        f"Multiple equally satisfactory handlers found ({compatible_handlers}). Using the last one added."
                    )
                target = compatible_handlers[-1]
            else:
                log.warn("No compatible handlers found. Raising the error.")
                raise e

            self.handlers[target].handle(
                error=e,
                invoker=self.executable,
                trace=traceback.format_exc(),
                inputs={"args": args, "kwargs": kwargs},
            )


class RaiserHandler(ErrorHandler, error=BioTeaError):
    """Handle that just raises the error. The 'non-handle'."""

    def handle(error: Exception, invoker: Callable, trace: str, inputs: dict) -> None:
        raise error


class UnsupportedOptionError(Exception):
    pass
