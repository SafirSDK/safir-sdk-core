cmake_minimum_required(VERSION 2.4)

add_subdirectory(boost)

#file (READ /etc/debian_versionasdf debian_version)
find_file(debian_version "debian_version" /etc/)
if (debian_version)
  add_subdirectory(debian_xmlada)
endif()

