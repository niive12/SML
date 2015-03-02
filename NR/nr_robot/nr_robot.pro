TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG += qt
CONFIG += std=c++11

SOURCES += main.cpp


include(deployment.pri)
qtcAddDeployment()

