/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mysema.scalagen

import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.expr.NameExpr
import com.mysema.scalagen.UnitTransformer._

/**
 * Annotations turns Annotation type declarations into normal classes which extend
 * StaticAnnotation
 */
class Annotations(targetVersion: ScalaVersion) extends UnitTransformerBase {
  
  private val staticAnnotationType = new ClassOrInterface("StaticAnnotation")
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: AnnotationDecl, arg: CompilationUnit) = {
    // turns annotations into StaticAnnotation subclasses
    arg.getImports().add(new ImportDeclaration(new NameExpr("scala.annotation.StaticAnnotation"), false, false))
    val clazz = new ClassOrInterfaceDecl()
    clazz.setName(n.getName)    
    clazz.setExtends(staticAnnotationType :: Nil)
    clazz.setMembers(createMembers(n))
    clazz
  }
  
  private def createMembers(n: AnnotationDecl): JavaList[BodyDecl] = {
    // TODO : default values
    val params = n.getMembers
      .collect { case m: AnnotationMember => m }
      .map(m => new Parameter(PROPERTY, m.getType, new VariableDeclaratorId(m.getName)))
      
    if (!params.isEmpty) {
      val constructor = new Constructor()
      constructor.setParameters(params)
      constructor.setBlock(new Block())
      constructor :: Nil
    } else {
      Nil
    }
  }
    
}  