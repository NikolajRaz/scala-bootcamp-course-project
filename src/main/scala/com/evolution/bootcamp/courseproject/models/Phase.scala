package com.evolution.bootcamp.courseproject.models

sealed trait Phase

case object BETS_OPENED extends Phase
case object BETS_CLOSED extends Phase
case object RESULT_ANNOUNCED extends Phase
