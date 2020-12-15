package models.writers

case class PAModelConfig(uri : String, leastX : Float,
                         decoupledZ : Boolean,
                         genRegulateTerminal : Boolean,
                         enableSvcVReg : Boolean, name : String)