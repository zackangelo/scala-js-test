package escalera

import escalera.router.{ Router, Dsl }

/**
 * Created by zackangelo on 1/11/15.
 */
trait Application
  extends Router
  with Dsl
