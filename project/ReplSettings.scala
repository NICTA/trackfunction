import sbt._
import Keys._

object ReplSettings {
  type Sett = Project.Setting[_]

  lazy val all = Seq[Sett](
    initialCommands := """
                         |import com.nicta.trackfunction._
                         |import TrackFunction._
                         |import scalaz._
                         |import Scalaz._
                       """.stripMargin
  )
}
