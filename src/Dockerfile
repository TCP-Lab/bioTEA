FROM cmalabscience/biotea-base:0.1.0

# Make the hardcoded mountpoints for the outputs and inputs
RUN mkdir /biotea && \
  mkdir /biotea/target && mkdir /biotea/input && mkdir /biotea/logs

WORKDIR /biotea

# Copy the source code
COPY ./biotea/ /biotea/

# Setup the entrypoint
ENTRYPOINT [ "/biotea/entrypoint.R" ]
