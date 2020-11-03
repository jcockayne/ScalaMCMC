import breeze.linalg.{DenseMatrix, DenseVector, InjectNumericOps, norm}
import org.scalatest.FunSuite
import breeze.stats
import org.scalatest.Matchers.convertNumericToPlusOrMinusWrapper

class RWMTest extends FunSuite {
  test("We get about the right mean and variance for a Gaussian distribution") {

    val proposal = stats.distributions.Gaussian(0, 1)
    val target = stats.distributions.Gaussian(3, 0.2)
    val kernel = new RWMKernel(proposal, target.logPdf)
    val samples = kernel.iterate(0.0).slice(1000, 5000)

    assert(stats.mean(samples) === target.mean +- 0.1)
    assert(stats.variance(samples) === target.variance +- 0.1)
  }

  test("This all works in multiple dimensions") {
    val proposal = stats.distributions.MultivariateGaussian(DenseVector.zeros(2), DenseMatrix.eye(2))

    val target = stats.distributions.MultivariateGaussian(3.0 * DenseVector.ones[Double](2), 0.2 * DenseMatrix.eye[Double](2))

    val kernel = new RWMKernel(proposal, target.logPdf)
    val samples = kernel.iterate(DenseVector.zeros[Double](2)).slice(1000, 5000).toArray

    assert(norm(Statistics.mean(samples) - target.mean) < 0.1)
    assert(MatrixNorms.norm(Statistics.covariance(samples) - target.covariance) < 0.1)
  }

  test("Check a more complex target") {

    val proposal = stats.distributions.Gaussian(0, 1)
    val a = 0.0
    val b = 1.0
    val mu = 0.5
    val sig = 0.1

    val gBase = stats.distributions.Gaussian(mu, sig)
    val alpha = (a - mu) / sig
    val beta = (b - mu) / sig
    val Z = gBase.cdf(beta) - gBase.cdf(alpha)

    val expectedMean = mu + (gBase.pdf(alpha) - gBase.pdf(beta)) / Z * sig
    val expectedVar = sig*sig *(1 + (alpha*gBase.pdf(alpha) - beta*gBase.pdf(beta)) / Z - (gBase.pdf(alpha) - gBase.pdf(beta)) / Z)

    val target = (x : Double) => x match {
      case x if x > a && x < b => stats.distributions.Gaussian(mu, sig).logPdf(x)
      case _ => Double.NegativeInfinity
    }

    val kernel = new RWMKernel(proposal, target)
    val samples = kernel.iterate(0.0).slice(1000, 5000)

    assert(stats.mean(samples) === expectedMean +- 0.1)
    assert(stats.variance(samples) === expectedVar +- 0.1)
  }
}