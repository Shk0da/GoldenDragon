package com.github.shk0da.GoldenDragon.utils;

import java.io.File;

public class NativeLoaderUtils {

    private static final String LIBS_DIR = "libs";

    public static void loadLibrary(String libraryName) {
        try {
            System.loadLibrary(libraryName);
            System.out.println("Loaded " + libraryName);
        } catch (UnsatisfiedLinkError e) {
            loadFromLibsFolder(libraryName);
        }
    }

    private static void loadFromLibsFolder(String libraryName) {
        try {
            String os = getOS();
            String arch = getArchitecture();

            String libFileName = System.mapLibraryName(libraryName);
            String libPath = LIBS_DIR + "/" + os + "/" + arch + "/" + libFileName;

            File libFile = new File(libPath);
            if (!libFile.exists()) {
                libPath = LIBS_DIR + "/" + os + "/" + libFileName;
                libFile = new File(libPath);
            }

            if (!libFile.exists()) {
                libPath = LIBS_DIR + "/" + libFileName;
                libFile = new File(libPath);
            }

            if (!libFile.exists()) {
                throw new UnsatisfiedLinkError("Not found " + libraryName + " in libs. Path: " + libPath);
            }

            String absolutePath = libFile.getAbsolutePath();
            System.out.println("Loading from: " + absolutePath);

            System.load(absolutePath);
            System.out.println(libraryName + " loaded successfully");

        } catch (Exception ex) {
            throw new UnsatisfiedLinkError("Failed load " + libraryName + " from libs: " + ex.getMessage());
        }
    }

    private static String getOS() {
        String osName = System.getProperty("os.name").toLowerCase();
        if (osName.contains("win")) return "windows";
        if (osName.contains("mac")) return "macos";
        if (osName.contains("linux")) return "linux";
        if (osName.contains("nix")) return "unix";
        return "unknown";
    }

    private static String getArchitecture() {
        String arch = System.getProperty("os.arch").toLowerCase();
        if (arch.contains("64")) return "x86_64";
        if (arch.contains("86")) return "x86";
        if (arch.contains("arm")) return "arm";
        return "unknown";
    }
}