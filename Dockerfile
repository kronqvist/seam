# Base image
FROM debian:stable-slim

# Install dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates autoconf \
    cmake \
    curl \
    firefox-esr \
    dbus-x11 \
    libcanberra-gtk3-0 \
    fop \
    git \
    openssh-client \
    tcpdump \
    libncurses5-dev \
    libreadline-dev \
    libsctp-dev \
    libssl-dev \
    libwxgtk3.2-dev \
    libxml2-dev \
    libxslt-dev \
    wget \
    cloc \
    gnupg && \
    rm -rf /var/lib/apt/lists/*

# Install Node.js 20, Gemini CLI, Codex
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
    npm install -g @google/gemini-cli @openai/codex && \
    rm -rf /var/lib/apt/lists/*

# Install Erlang/OTP 27.2
ENV ERLANG_VERSION=27.2
RUN wget https://github.com/erlang/otp/releases/download/OTP-${ERLANG_VERSION}/otp_src_${ERLANG_VERSION}.tar.gz && \
    tar -xzf otp_src_${ERLANG_VERSION}.tar.gz && \
    cd otp_src_${ERLANG_VERSION} && \
    ./configure && make -j$(nproc) && make install && \
    cd .. && rm -rf otp_src_${ERLANG_VERSION} otp_src_${ERLANG_VERSION}.tar.gz

# Clone and build Lexbor
WORKDIR /app/csrc
RUN git clone https://github.com/lexbor/lexbor.git
RUN cd lexbor && \
    cmake . && \
    make -j$(nproc) && \
    make install && \
    ldconfig

# Set working directory and install rebar3
WORKDIR /app
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && mv rebar3 /usr/local/bin/

# Add user
ARG USER_ID
ARG GROUP_ID
ARG USERNAME
RUN groupadd -g ${GROUP_ID} ${USERNAME} && \
    useradd -u ${USER_ID} -g ${GROUP_ID} -m ${USERNAME} && \
    mkdir -p /app/.cache && chown -R ${USERNAME}:${USERNAME} /app

# Set the default user
USER ${USERNAME}

# Install Claude - must be installed after switching to user
RUN curl -fsSL https://claude.ai/install.sh | bash

# Add Claude to PATH
ENV PATH="/home/${USERNAME}/.local/bin:${PATH}"

CMD ["rebar3", "shell"]