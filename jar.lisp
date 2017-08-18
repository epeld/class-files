
(ql:quickload :zip)
(ql:quickload :flexi-streams)

(defun suffixp (suffix string)
  "Return true if STRING is suffixed by SUFFIX"
  (search suffix string :from-end t))

(defparameter
    test-classes
    (zip:with-zipfile (z "~/jedit.jar")
      (let (classes)
        (zip:do-zipfile-entries (name entry z)
          (when (suffixp ".class" name)
            (format t "Reading class ~a~%" name)
            (let ((stream (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents entry))))
              (push (parse-class-stream stream) classes))))
        classes)))


;; flexi-streams in-memory streams!

(defvar example-contents
  '("META-INF/INDEX.LIST" "installer/jedit-windows.tar.bz2"
    "installer/jedit-program.tar.bz2" "installer/jedit-os2.tar.bz2"
    "installer/jedit-macros.tar.bz2" "installer/jedit-mac.tar.bz2"
    "installer/jedit-api.tar.bz2" "jedit.1" "installer/readme.html"
    "installer/gpl.html" "installer/done-Windows.html" "installer/done-VMS.html"
    "installer/done-Unix.html" "installer/done-MacOS.html"
    "installer/done-HalfAnOS.html" "installer/install.props"
    "installer/TarOutputStream.class" "installer/TarInputStream.class"
    "installer/TarInputStream$EntryFactory.class"
    "installer/TarInputStream$EntryAdapter.class" "installer/TarHeader.class"
    "installer/TarEntry.class" "installer/TarBuffer.class"
    "installer/SwingInstall.class" "installer/SwingInstall$WizardLayout.class"
    "installer/SwingInstall$WindowHandler.class"
    "installer/SwingInstall$TextPanel.class"
    "installer/SwingInstall$SwingProgress.class"
    "installer/SwingInstall$SwingProgress$5.class"
    "installer/SwingInstall$SwingProgress$4.class"
    "installer/SwingInstall$SwingProgress$3.class"
    "installer/SwingInstall$SwingProgress$2.class"
    "installer/SwingInstall$SwingProgress$1.class"
    "installer/SwingInstall$SelectComponents.class"
    "installer/SwingInstall$DirVerifier.class"
    "installer/SwingInstall$DirVerifier$2.class"
    "installer/SwingInstall$DirVerifier$1.class"
    "installer/SwingInstall$ChooseDirectory.class"
    "installer/SwingInstall$ChooseDirectory$ActionHandler.class"
    "installer/SwingInstall$ChooseDirectory$1.class"
    "installer/SwingInstall$ActionHandler.class" "installer/ServerKiller.class"
    "installer/Progress.class" "installer/OperatingSystem.class"
    "installer/OperatingSystem$Windows.class"
    "installer/OperatingSystem$Windows$JEditLauncherOSTask.class"
    "installer/OperatingSystem$VMS.class" "installer/OperatingSystem$Unix.class"
    "installer/OperatingSystem$Unix$ScriptOSTask.class"
    "installer/OperatingSystem$Unix$ManPageOSTask.class"
    "installer/OperatingSystem$OSTask.class"
    "installer/OperatingSystem$MacOS.class"
    "installer/OperatingSystem$HalfAnOS.class"
    "installer/NonInteractiveInstall.class"
    "installer/InvalidHeaderException.class" "installer/InstallThread.class"
    "installer/Install.class" "installer/ConsoleProgress.class"
    "installer/ConsoleInstall.class" "installer/CRC.class"
    "installer/CBZip2OutputStream.class"
    "installer/CBZip2OutputStream$StackElem.class"
    "installer/CBZip2OutputStream$1.class" "installer/CBZip2InputStream.class"
    "installer/BZip2Constants.class" "installer/" "META-INF/MANIFEST.MF"
    "META-INF/"))
