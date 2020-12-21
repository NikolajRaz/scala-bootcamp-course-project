package com.evolution.bootcamp.courseproject.models

sealed trait Request

case object PLACE_BET extends Request
case object REMOVE_BET extends Request
