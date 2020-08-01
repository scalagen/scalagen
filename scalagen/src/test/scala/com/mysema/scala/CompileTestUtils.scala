package com.mysema.scala

import scala.tools.nsc.interpreter.IMain
import scala.io.Source.fromFile
import java.io.File
import java.net.URL
import scala.tools.nsc.Settings

object CompileTestUtils {
  import java.io.File.pathSeparator
  
  val currentLibraries = this.getClass.getClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs().toList
  val compilerPath = jarPathOfClass("scala.tools.nsc.Interpreter")
  val libraryPath = jarPathOfClass("scala.Some")

  val cp = currentLibraries :+ compilerPath :+ libraryPath
  val cpString = cp.mkString(pathSeparator)
     
  private def jarPathOfClass(className: String): URL = {
    Class.forName(className).getProtectionDomain.getCodeSource.getLocation    
  }
}

trait CompileTestUtils {
  import CompileTestUtils._ 
  import java.io.File.pathSeparator

  def assertCompileSuccess(files: Traversable[File]): Unit = {
    assertCompileSuccess(files
                           .map(fromFile(_).mkString)
                           .mkString("\n"))
  }
  
  def assertCompileSuccess(source: String, settingsTransformation: Settings => Settings = identity): Unit = {
    val out = new java.io.ByteArrayOutputStream
    val interpreterWriter = new java.io.PrintWriter(out)
    
    val untransformedEnv = new Settings()
    untransformedEnv.bootclasspath.value = List(untransformedEnv.bootclasspath.value, cpString).mkString(pathSeparator)
    val env = settingsTransformation(untransformedEnv)
    // The next line causes a problem like this "class StringContext does not have a member f"
    // env.usejavacp.value = true

    val interpreter = new IMain(env, interpreterWriter)
    try {
      val result = interpreter.interpret(source.replaceAll("package ", "import "))
      //we have to compare as a string because of an incompatibility between 2.9 and 2.10:
      //in 2.9, result is scala.tools.nsc.InterpreterResults
      //in 2.10, result is scala.tools.nsc.interpreter.Results
      if (result.toString != "Success") {
        throw new AssertionError("Compile failed, interpreter output:\n" + out.toString("utf-8") + " for the following code\n" + source)
      }
    } finally {
      interpreterWriter.close
      interpreter.close
    }
  }
  
  def recursiveFileList(file: File): Array[File] = {
    if (file.isDirectory) {
      file.listFiles.flatMap(recursiveFileList(_))
    } else {
      Array(file)
    }
  }  
}
