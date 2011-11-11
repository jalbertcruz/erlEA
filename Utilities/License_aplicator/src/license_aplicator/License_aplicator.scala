package license_aplicator

object License_aplicator extends App {
	//val targets = List(("config.clj", "masterSlaveModel.config/configuration"))
	val obj = new Clj2src(args(0), args(1))
    obj apply_license
}