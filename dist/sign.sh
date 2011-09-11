#!/bin/sh

jarsigner -keystore .keystore -storepass password $1 myself
