package com.evolution.bootcamp.courseproject.models

sealed trait BetType
case object SINGLE extends BetType
case object SPLIT extends BetType
case object STREET extends BetType
case object SQUARE extends BetType
case object DOUBLE_STREET extends BetType
case object BASKET extends BetType
case object FIRST_FOUR extends BetType
case object RED_NUMBERS extends BetType
case object BLACK_NUMBERS extends BetType
case object EVEN extends BetType
case object ODD extends BetType
case object SMALL extends BetType
case object BIG extends BetType
case object DOZEN extends BetType
case object ROW extends BetType
