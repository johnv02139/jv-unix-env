# -*- Mode: sh -*-

# User configuration

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

export QUALTRICSHOSTNAME=ops.b1-prv.local.johnv
launchctl setenv QUALTRICSHOSTNAME ops.b1-prv.local.johnv

export JAVA_HOME=`/usr/libexec/java_home -v 1.8`

export DB_USER=qualtrics2016
export AWS_REGION=us-west-2
export STANDALONE_DB_ADMIN_USER=actpadmin
export MONOLITH_CLIENT_ID=Qualtrics360
export ARTIFACTORY_USER=developer

launchctl setenv AWS_REGION us-west-2

export SCHEMA_REGISTRY_URL=localhost:8881
export QPL_BULK_BOOTSTRAP_SERVERS=localhost:9192
export QPL_DURABLE_BOOTSTRAP_SERVERS=localhost:9192
export QCL_BULK_BOOTSTRAP_SERVERS=localhost:9192
export QCL_BACKGROUND_JOB_BOOTSTRAP_SERVERS=localhost:9192
export QCL_IDEMPOTENT_BACKGROUND_JOB_BOOTSTRAP_SERVERS=localhost:9192

export TELEGRAF_PORT=8021
export LOG_DIRECTORY=${HOME}/Documents/Logs

export NVM_DIR="${HOME}/.nvm"
[ -s "${NVM_DIR}/nvm.sh" ] && \. "${NVM_DIR}/nvm.sh"  # This loads nvm
nvm use 10
