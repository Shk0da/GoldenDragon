package com.github.shk0da.goldendragon.archunit;

import com.tngtech.archunit.core.domain.JavaClass;
import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.domain.JavaField;
import com.tngtech.archunit.core.domain.JavaMethod;
import com.tngtech.archunit.core.domain.JavaModifier;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.core.importer.ImportOption;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * ArchUnit тесты для обнаружения потенциального мёртвого кода.
 */
class DeadCodeArchTest {

    private static final Set<String> ALLOWED_EMPTY_CLASSES = new HashSet<>(Arrays.asList(
            "com.github.shk0da.goldendragon.GoldenDragon"
    ));

    private final JavaClasses allClasses = new ClassFileImporter()
            .importPackages("com.github.shk0da.goldendragon");

    private boolean isTestClass(JavaClass clazz) {
        String name = clazz.getFullName();
        if (name.endsWith("Test") || name.endsWith("Tests") || name.contains(".test.")) {
            return true;
        }
        // Also check source path for test classes
        String source = clazz.getSource().map(s -> s.getUri().toString()).orElse("");
        return source.contains("/test/") || source.contains("\\test\\");
    }

    private boolean isFromTestClass(com.tngtech.archunit.core.domain.JavaAccess<?> access) {
        return isTestClass(access.getOriginOwner());
    }

    @Test
    void shouldDetectUnusedClasses() {
        Set<JavaClass> unused = allClasses.stream()
                .filter(c -> !isTestClass(c))
                .filter(c -> !c.isAnonymousClass())
                .filter(c -> !c.getModifiers().contains(JavaModifier.ABSTRACT) && !c.isInterface())
                .filter(c -> !c.isAnnotation())
                .filter(c -> !c.isEnum())
                .filter(c -> !ALLOWED_EMPTY_CLASSES.contains(c.getFullName()))
                .filter(c -> c.getAccessesToSelf().stream()
                        .noneMatch(a -> !isFromTestClass(a)))
                .collect(Collectors.toSet());

        assertThat(unused)
                .withFailMessage(() -> {
                    StringBuilder sb = new StringBuilder("Классы без ссылок из production-кода:\n");
                    for (JavaClass c : unused) {
                        sb.append("  - ").append(c.getFullName()).append("\n");
                    }
                    return sb.toString();
                })
                .isEmpty();
    }

    @Test
    void shouldDetectUnusedPrivateMethods() {
        Set<JavaMethod> unused = allClasses.stream()
                .flatMap(c -> c.getMethods().stream())
                .filter(m -> !isTestClass(m.getOwner()))
                .filter(m -> m.getModifiers().contains(JavaModifier.PRIVATE))
                .filter(m -> !m.getModifiers().contains(JavaModifier.ABSTRACT))
                .filter(m -> !m.getName().startsWith("access$"))
                .filter(m -> !m.getName().equals("<clinit>"))
                .filter(m -> !m.getName().equals("<init>"))
                .filter(m -> m.getAccessesToSelf().stream()
                        .noneMatch(a -> !isFromTestClass(a)))
                .collect(Collectors.toSet());

        assertThat(unused)
                .withFailMessage(() -> {
                    StringBuilder sb = new StringBuilder("Неиспользуемые private методы (нет вызовов из production):\n");
                    for (JavaMethod m : unused) {
                        sb.append("  - ").append(m.getOwner().getFullName())
                                .append("#").append(m.getName()).append("()\n");
                    }
                    return sb.toString();
                })
                .isEmpty();
    }

    @Test
    void shouldDetectUnusedPackagePrivateMethods() {
        Set<JavaMethod> unused = allClasses.stream()
                .flatMap(c -> c.getMethods().stream())
                .filter(m -> !isTestClass(m.getOwner()))
                .filter(m -> !m.getModifiers().contains(JavaModifier.PUBLIC)
                        && !m.getModifiers().contains(JavaModifier.PRIVATE)
                        && !m.getModifiers().contains(JavaModifier.PROTECTED))
                .filter(m -> !m.getModifiers().contains(JavaModifier.ABSTRACT))
                .filter(m -> m.getAccessesToSelf().stream()
                        .noneMatch(a -> !isFromTestClass(a)))
                .collect(Collectors.toSet());

        assertThat(unused)
                .withFailMessage(() -> {
                    StringBuilder sb = new StringBuilder("Неиспользуемые package-private методы (нет вызовов из production):\n");
                    for (JavaMethod m : unused) {
                        sb.append("  - ").append(m.getOwner().getFullName())
                                .append("#").append(m.getName()).append("()\n");
                    }
                    return sb.toString();
                })
                .isEmpty();
    }

    @Test
    void shouldDetectUnusedPublicMethods() {
        Set<JavaMethod> unused = allClasses.stream()
                .flatMap(c -> c.getMethods().stream())
                .filter(m -> !isTestClass(m.getOwner()))
                .filter(m -> m.getModifiers().contains(JavaModifier.PUBLIC))
                .filter(m -> !m.getModifiers().contains(JavaModifier.ABSTRACT))
                .filter(m -> !isIgnoredPublicMethod(m))
                .filter(m -> m.getAccessesToSelf().stream()
                        .noneMatch(a -> !isFromTestClass(a)))
                .collect(Collectors.toSet());

        assertThat(unused)
                .withFailMessage(() -> {
                    StringBuilder sb = new StringBuilder("Неиспользуемые public методы (нет вызовов из production):\n");
                    for (JavaMethod m : unused) {
                        sb.append("  - ").append(m.getOwner().getFullName())
                                .append("#").append(m.getName()).append("()\n");
                    }
                    return sb.toString();
                })
                .isEmpty();
    }

    private boolean isIgnoredPublicMethod(JavaMethod method) {
        String name = method.getName();
        if (name.equals("main") || name.equals("valueOf") || name.equals("values")) {
            return true;
        }
        if (name.equals("toString") || name.equals("hashCode") || name.equals("equals")) {
            return true;
        }
        JavaClass owner = method.getOwner();
        if (owner.isInterface() || owner.isEnum()) {
            return true;
        }
        if (method.getOwner().getRawInterfaces().stream()
                .anyMatch(iface -> iface.getMethods().stream()
                        .anyMatch(m -> m.getName().equals(name)))) {
            return true;
        }
        return false;
    }

    @Test
    void shouldDetectUnusedPrivateFinalFields() {
        Set<JavaField> unused = allClasses.stream()
                .flatMap(c -> c.getFields().stream())
                .filter(f -> !isTestClass(f.getOwner()))
                .filter(f -> f.getModifiers().contains(JavaModifier.PRIVATE))
                .filter(f -> f.getModifiers().contains(JavaModifier.FINAL))
                .filter(f -> !f.getName().equals("serialVersionUID"))
                .filter(f -> !f.getName().startsWith("LOG"))
                .filter(f -> !f.getName().startsWith("logger"))
                .filter(f -> f.getAccessesToSelf().stream()
                        .noneMatch(a -> !isFromTestClass(a) && a.getAccessType() == com.tngtech.archunit.core.domain.JavaFieldAccess.AccessType.GET))
                .collect(Collectors.toSet());

        assertThat(unused)
                .withFailMessage(() -> {
                    StringBuilder sb = new StringBuilder("Неиспользуемые private final поля (нет чтений из production):\n");
                    for (JavaField f : unused) {
                        sb.append("  - ").append(f.getOwner().getFullName())
                                .append(" -> ").append(f.getName()).append("\n");
                    }
                    return sb.toString();
                })
                .isEmpty();
    }

    @Test
    void shouldDetectUnusedPrivateFields() {
        Set<JavaField> unused = allClasses.stream()
                .flatMap(c -> c.getFields().stream())
                .filter(f -> !isTestClass(f.getOwner()))
                .filter(f -> f.getModifiers().contains(JavaModifier.PRIVATE))
                .filter(f -> !f.getModifiers().contains(JavaModifier.FINAL))
                .filter(f -> !f.getName().startsWith("LOG"))
                .filter(f -> !f.getName().startsWith("logger"))
                .filter(f -> f.getAccessesToSelf().stream()
                        .noneMatch(a -> !isFromTestClass(a) && a.getAccessType() == com.tngtech.archunit.core.domain.JavaFieldAccess.AccessType.GET))
                .collect(Collectors.toSet());

        assertThat(unused)
                .withFailMessage(() -> {
                    StringBuilder sb = new StringBuilder("Неиспользуемые private поля (нет чтений из production):\n");
                    for (JavaField f : unused) {
                        sb.append("  - ").append(f.getOwner().getFullName())
                                .append(" -> ").append(f.getName()).append("\n");
                    }
                    return sb.toString();
                })
                .isEmpty();
    }

    @Test
    void shouldDetectUnassignedPrivateFields() {
        Set<JavaField> unassigned = allClasses.stream()
                .flatMap(c -> c.getFields().stream())
                .filter(f -> !isTestClass(f.getOwner()))
                .filter(f -> f.getModifiers().contains(JavaModifier.PRIVATE))
                .filter(f -> !f.getModifiers().contains(JavaModifier.FINAL))
                .filter(f -> !f.getName().startsWith("LOG"))
                .filter(f -> !f.getName().startsWith("logger"))
                .filter(f -> f.getAccessesToSelf().stream()
                        .noneMatch(a -> !isFromTestClass(a) && a.getAccessType() == com.tngtech.archunit.core.domain.JavaFieldAccess.AccessType.SET))
                .collect(Collectors.toSet());

        assertThat(unassigned)
                .withFailMessage(() -> {
                    StringBuilder sb = new StringBuilder("Неприсваиваемые private поля (нет SET из production):\n");
                    for (JavaField f : unassigned) {
                        sb.append("  - ").append(f.getOwner().getFullName())
                                .append(" -> ").append(f.getName()).append("\n");
                    }
                    return sb.toString();
                })
                .isEmpty();
    }

    @Test
    void shouldDetectUnusedConstructors() {
        Set<com.tngtech.archunit.core.domain.JavaConstructor> unused = allClasses.stream()
                .flatMap(c -> c.getConstructors().stream())
                .filter(ctor -> !isTestClass(ctor.getOwner()))
                .filter(ctor -> ctor.getConstructorCallsFromSelf().stream()
                        .noneMatch(call -> !isFromTestClass(call)))
                .collect(Collectors.toSet());

        assertThat(unused)
                .withFailMessage(() -> {
                    StringBuilder sb = new StringBuilder("Неиспользуемые конструкторы (нет вызовов из production):\n");
                    for (com.tngtech.archunit.core.domain.JavaConstructor c : unused) {
                        sb.append("  - ").append(c.getOwner().getFullName())
                                .append("#").append(c.getName()).append("()\n");
                    }
                    return sb.toString();
                })
                .isEmpty();
    }
}
