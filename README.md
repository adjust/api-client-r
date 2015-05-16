# R Client for the Adjust Statistics API

## Introduction

    > library(adjust)

    > adjust.setup(user_token='aYSsuEVhAMDQDyZ8kj2K', app_token='abcdefg')

    > cohorts <- adjust.cohorts(countries=c('us', 'gb'), os_names=c('ios'), device_types=c('panasonic'))

    > deliverables <- adjust.deliverables(countries=c('us', 'gb'), os_names=c('ios'), device_types=c('panasonic'))

    > events <- adjust.events(countries=c('us', 'gb'), os_names=c('ios'), device_types=c('panasonic'))
