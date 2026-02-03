git clone https://github.com/tdlib/td.git
cd td
git clone https://github.com/Microsoft/vcpkg.git
cd vcpkg
git checkout bc3512a509f9d29b37346a7e7e929f9a26e66c7e
./bootstrap-vcpkg.bat
./vcpkg.exe install gperf:x86-windows openssl:x86-windows zlib:x86-windows
cd ..
Remove-Item build -Force -Recurse -ErrorAction SilentlyContinue
mkdir build
cd build
cmake -A Win32 -DCMAKE_INSTALL_PREFIX:PATH=../example/java/td -DTD_ENABLE_JNI=ON -DCMAKE_TOOLCHAIN_FILE:FILEPATH=../vcpkg/scripts/buildsystems/vcpkg.cmake ..
cmake --build . --target install --config Release
cd ..
cd example/java
Remove-Item build -Force -Recurse -ErrorAction SilentlyContinue
mkdir build
cd build
cmake -A Win32 -DCMAKE_INSTALL_PREFIX:PATH=../../../tdlib -DCMAKE_TOOLCHAIN_FILE:FILEPATH=../../../vcpkg/scripts/buildsystems/vcpkg.cmake -DTd_DIR:PATH=$(Resolve-Path ../td/lib/cmake/Td) ..
cmake --build . --target install --config Release
cd ../../..
cd ..
dir td/tdlib

mkdir libs
cp td/tdlib/bin/*.dll libs
echo "Library saved to project directory: libs"

rm -rf td
echo "Build process completed successfully!"