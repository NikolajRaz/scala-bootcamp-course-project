package com.evolution.bootcamp.courseproject

sealed trait Phase

case object BETS_OPEN extends Phase
case object BETS_CLOSED extends Phase
case object RESULT_ANNOUNCED extends Phase
