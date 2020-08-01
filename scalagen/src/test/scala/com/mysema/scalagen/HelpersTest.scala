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

import com.mysema.scalagen.UnitTransformer._
import java.util.Collections
import org.junit.Assert._
import org.junit.Test

class HelpersTest {
  
  val helpers = new AnyRef with Helpers
  
  @Test
  def IsHashCode {
    val method = new Method(0, Type.Int, "hashCode", null)
    assertTrue(helpers.isHashCode(method))
  }
  
  @Test
  def IsEquals {
    val method = new Method(0, Type.Boolean, "equals", Collections.singletonList[Parameter](new Parameter))
    assertTrue(helpers.isEquals(method))
  }
  
  @Test
  def ToString {
    val method = new Method(0, Type.String, "toString", null)
    assertTrue(helpers.isToString(method))
  }
  
  
}