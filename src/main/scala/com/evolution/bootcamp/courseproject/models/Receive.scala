package com.evolution.bootcamp.courseproject.models

sealed trait Receive

case object PLACE_BET extends Receive
case object REMOVE_BET extends Receive
