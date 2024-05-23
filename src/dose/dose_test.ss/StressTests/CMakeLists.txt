cmake_minimum_required(VERSION 3.27)

ADD_SUBDIRECTORY(EntityIntegrityChecker)
ADD_SUBDIRECTORY(EntityReceptionVerifier)
ADD_SUBDIRECTORY(EntityStresser)
ADD_SUBDIRECTORY(MessageLatencies)
ADD_SUBDIRECTORY(MessageStresser)

ADD_SUBDIRECTORY(RequestLatencies)
ADD_SUBDIRECTORY(RequestStresser)

ADD_SUBDIRECTORY(MultiPingPong)

ADD_SUBDIRECTORY(RequestPingPong)

