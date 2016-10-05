#!/usr/bin/env sh

PROTOC_VERSION="3.0.2"

if [[ "$(protoc --version)" != *${PROTOC_VERSION}* ]]; then
    wget https://github.com/google/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-linux-x86_64.zip
    unzip protoc-${PROTOC_VERSION}-linux-x86_64.zip
    cp protoc-${PROTOC_VERSION}-linux-x86_64/bin/protoc .local/bin/
else
    echo "$(protoc --version) already installed"
fi
