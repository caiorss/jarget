/** 
     - Author: Caio Rodrigues <caiorss [DOT] rodrigues [AT] gmail [DOT] com>

    This is free and unencumbered software released into the public domain.

    Anyone is free to copy, modify, publish, use, compile, sell, or
    distribute this software, either in source code form or as a compiled
    binary, for any purpose, commercial or non-commercial, and by any
    means.

    In jurisdictions that recognize copyright laws, the author or authors
    of this software dedicate any and all copyright interest in the
    software to the public domain. We make this dedication for the benefit
    of the public at large and to the detriment of our heirs and
    successors. We intend this dedication to be an overt act of
    relinquishment in perpetuity of all present and future rights to this
    software under copyright law.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
    OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
    ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    OTHER DEALINGS IN THE SOFTWARE.  

 =============================================================== */

package jarget.reader

case class Reader[R, A](run: R => A){
  
  // Haskell fmap or <$> operator
  //
  def map[B](fn: A => B): Reader[R, B] = 
    Reader(r => fn(this.run(r)))

  // Haskell bind (>>=)
  //
  def flatMap[B](fn: A => Reader[R, B]): Reader[R, B] =
    Reader(r => fn(this.run(r)).run(r))

  def withReader[Z](fn: Z => R): Reader[Z, A] =
    Reader(z => this.run(fn(z)))

  def local(fn: R => R): Reader[R, A] =
    Reader(r => this.run(fn(r)))

  def foreach(fn: A => Unit): Reader[R, Unit] =
    Reader(r => fn(this.run(r)))

  // Haskell - runReader 
  def apply(x: R): A = run(x)  
}

object Reader{

  // Equivalent of Haskell return :: a -> (Reader r) a
  def pure[R, A](x: A) = Reader((_: R) => x)

  def ask[R] = Reader[R, R]((r: R) => r)

  def liftIO[R, A](io: => A) = Reader((r: R) => io)
}
