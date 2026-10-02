#!/bin/bash
# Builds "Groovehaus Darkroom.app" and a .dmg. Run on a Mac (the JavaFX / OpenCV /
# LibRaw natives are picked per OS, so the app can only be built on the OS it runs on).
# Needs JDK 21 (jpackage) and Leiningen:  brew install --cask temurin@21 && brew install leiningen
set -euo pipefail
cd "$(dirname "$0")/.."
[ "$(uname)" = "Darwin" ] || { echo "run this on macOS"; exit 1; }

VERSION="${VERSION:-1.0.0}"
OUT=dist; rm -rf "$OUT" target/icon.iconset target/icon.icns; mkdir -p "$OUT"

lein uberjar
JAR=$(ls target/*-standalone.jar | head -1)

# icon from the medallion logo
mkdir target/icon.iconset
for s in 16 32 64 128 256 512; do
  sips -z $s $s resources/logo-medallion-yellow.png --out target/icon.iconset/icon_${s}x${s}.png >/dev/null
  sips -z $((s*2)) $((s*2)) resources/logo-medallion-yellow.png --out target/icon.iconset/icon_${s}x${s}@2x.png >/dev/null
done
iconutil -c icns target/icon.iconset -o target/icon.icns

mkdir -p target/jpkg && cp "$JAR" target/jpkg/app.jar
jpackage --type dmg --name "Groovehaus Darkroom" --app-version "$VERSION" \
  --input target/jpkg --main-jar app.jar --main-class darkroom.main \
  --icon target/icon.icns --dest "$OUT" \
  --java-options "-Djava.awt.headless=true" --java-options "-XX:MaxRAMPercentage=60" \
  --mac-package-identifier com.groovehaus.darkroom
ls -la "$OUT"
