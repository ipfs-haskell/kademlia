-- Parametrize over String
module TypeSpecification where

data PingMessage = PING | PONG

import Network.Socket (SockAddr(..), PortNumber, inet_ntoa, inet_addr)

class UDPPeer a where
	host :: a -> String
	port :: a -> PortNumber
	toUDPPeer :: SockAddr -> IO (Maybe a)

class XorDistance a where
	xor :: a -> a -> Ordering

class (XorDistance b, UDPPeer c) => Node a where
	udpPeer :: a -> c
	nodeId :: a -> b

sortByXor :: (Node a, XorDistance b) => [a] -> b -> [a]
sortByXor bucket id = unpack . sort . pack $ bucket
	where pack bk = zip bk $ map f bk
		f = xor id . nodeId
		sort = sortBy (compare `on` snd)
		unzip = map fst


class (Node b, Command c) Signal a where
	source :: a -> b
	command :: a -> c

data Command i a b =
	-- The basic PING and PONG
	PING
	| PONG
	-- Command to store a datablock `b` on a Node with id `i`
	| STORE i b
	-- Command to find a node with id `i`
	| FIND_NODE i
	-- Response to FIND_NODE
	| RETURN_NODES i [a]
	-- Command to find a datablock with key `i`
	| FIND_VALUE i
	-- Response to FIND_VALUE
	| RETURN_VALUE i b

