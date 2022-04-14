FROM cmalabscience/biotea-base:0.1.0

# Make the hardcoded mountpoints for the outputs and inputs
RUN mkdir /bioTEA && \
  mkdir /bioTEA/target && mkdir /bioTEA/input && mkdir /bioTEA/logs

WORKDIR /bioTEA

# Copy the source code
COPY ./box/ /bioTEA/

# Setup the entrypoint
ENTRYPOINT [ "/bioTEA/entrypoint.R" ]
