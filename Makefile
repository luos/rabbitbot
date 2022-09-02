PROJECT = rabbitbot
PROJECT_DESCRIPTION = "A tiny bot to run RabbitMQ commands"
PROJECT_VERSION = 1.0.0

DEPS = wire erlmachine syn

TEST_DEPS = meck

dep_wire = git https://github.com/erlmachinedev/wire

dep_erlmachine = git https://github.com/erlmachine/erlmachine

dep_syn = hex 3.2.0

dep_meck = git https://github.com/eproxus/meck.git 0.9.0

BUILD_DEPS += relx
include erlang.mk
