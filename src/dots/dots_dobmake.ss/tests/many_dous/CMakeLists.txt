cmake_minimum_required(VERSION 2.8.12)

find_package(SafirSDKCore REQUIRED)

#This is a testcase that generates lots of dou files to check
#that long command lines etc work.

foreach (i RANGE 1000)
  file (WRITE "ManyDous.Object${i}.dou"
    "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
    "<class xmlns=\"urn:safir-dots-unit\">\n"
    "  <name>ManyDous.Object${i}</name>\n"
    "  <baseClass>Object</baseClass>\n"
    "  <members>\n"
    "    <member>\n"
    "      <name>MyInt</name>\n"
    "      <type>Int32</type>\n"
    "    </member>\n"
    "  </members>\n"
    "</class>")
endforeach()

ADD_SAFIR_GENERATED_LIBRARY(
  NAME ManyDous
  GLOB)
