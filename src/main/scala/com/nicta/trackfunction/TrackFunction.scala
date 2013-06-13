package com.nicta
package trackfunction

trait TrackFunctionFunctions
  extends TrackFunctions
  with TrackResultFunctions
  with TrackResultsFunctions
  with TrackRunFunctions

trait TrackFunctionInstances
  extends TrackInstances
  with TrackResultInstances
  with TrackResultsInstances
  with TrackRunInstances

object TrackFunction extends TrackFunctionFunctions with TrackFunctionInstances