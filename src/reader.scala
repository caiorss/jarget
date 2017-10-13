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
