<?php

class Script {
	private $handler;

	public function __construct($path, $skip = 0) {
		$this->handler = fopen($path, 'r');
		for($i = 0; $i < $skip; $i++)
			fgets($this->handler);
	}

	public function line() {
		if(!$this->handler)
			return null;
		if(feof($this->handler)) {
			fclose($this->handler);
			return null;
		}

		$line = trim(fgets($this->handler));
		return !empty($line) ? $line : $this->line();
	}
}

function item_map(\PDO $db, $s_pch) {
	$map = [];
	$file = new Script($s_pch);
	while(!is_null($line = $file->line())) {
		$temp = null;
		if(preg_match('/^\[([0-9A-Za-z_:*~\'\.\-]+)\]\s*=\s*(\d+)$/', $line, $temp))
			$map[$temp[1]] = $temp[2];
		else
			echo 'Parsing error on line:'.PHP_EOL.$line.PHP_EOL;
	}
	return $map;
}

function parse_npc($line) {
	$temp = preg_split('/\t+/', $line);
	if(empty($temp))
		return null;
	if(end($temp) != 'npc_end' || reset($temp) != 'npc_begin')
		return null;

	$data = [
		'id' => $temp[2],
		'name' => substr($temp[3], 1, -1),
		'type' => $temp[1],
	];
	foreach(array_slice($temp, 4, -1) as $param) {
		list($key, $value) = explode('=', $param, 2);
		if(!empty($key) && trim($value) !== '')
			$data[$key] = trim($value);
		else
			throw new \Exception('Parsing error on parameter: '.$param);
	}

	$data['clan'] = array_filter(array_map(
		function($clan) { return $clan == '-1' ? null : substr($clan, 1);},
		explode(';', substr($data['clan'], 1, -1))));

	return $data;
}

function process_npcs(\PDO $db, $s_names, $s_data) {
	echo 'Processing npcs.'.PHP_EOL;
	if($s_names) {
		$db->query('DELETE FROM npc');

		$file = new Script($s_names, 1);
		while(!is_null($line = $file->line())) {
			$temp = null;
			if(preg_match('/^(\d+)\s+a,(.+?)\\\0/', $line, $temp)) {
				$query = $db->prepare('INSERT INTO npc(id, name, level, type, aggro) values(:id, :name, 0, -1, -1)');
				$query->bindValue(':id', $temp[1]);
				$query->bindValue(':name', $temp[2]);
				$query->execute();
			}
			else
				echo 'Parsing error on line:'.PHP_EOL.$line.PHP_EOL;
		}
	}

	if($s_data) {
		$file = new Script($s_data);
		while(!is_null($line = $file->line())) {
			if($data = parse_npc($line)) {
				switch(true) {
					case in_array($data['type'], ['warrior', 'treasure']): $type = 1; break;
					case $data['type'] == 'zzoldagu': $type = 2; break;
					case $data['type'] == 'boss': $type = 3; break;
					case in_array($data['type'], ['summon', 'pet']): $type = 4; break;
					default: $type = 0;
				}

				$query = $db->prepare('UPDATE npc SET level = :level, type = :type, aggro = :aggro, clan = :clan WHERE id = :id');
				$query->bindValue(':id', $data['id']);
				$query->bindValue(':level', $data['level']);
				$query->bindValue(':type', $type);
				if(!empty($data['agro_range']))
					$query->bindValue(':aggro', intval($data['agro_range']));
				else
					$query->bindValue(':aggro', null, \PDO::PARAM_NULL);
				if(!empty($data['clan']))
					$query->bindValue(':clan', implode(',', $data['clan']));
				else
					$query->bindValue(':clan', null, \PDO::PARAM_NULL);
				$query->execute();
			}
			else
				throw new \Exception('Parsing error on line: '.$line);
		}
	}

	$db->query('UPDATE npc SET name = \'Giant Basilisk\' WHERE id = 10352');
}

function parse_skill($line, $map) {
	$temp = preg_split('/\t+/', $line);
	if(empty($temp))
		return null;
	if(end($temp) != 'skill_end' || reset($temp) != 'skill_begin')
		return null;

	$data = [];
	foreach(array_slice($temp, 1, -1) as $param) {
		list($key, $value) = preg_split('/\s*=\s*/', $param, 2);
		if(!empty($key) && trim($value) !== '')
			$data[$key] = trim($value);
		else
			throw new \Exception('Parsing error on parameter: '.$param);
	}

	$data['id'] = intval($data['skill_id']);
	$data['level'] = intval($data['level']);
	$data['name'] = substr($data['skill_name'], 1, -1);
	$temp = null;
	if($param = $data['item_consume'] ?? null)
		if(preg_match('/^{\[([0-9a-z_]+)\];(\d+)}$/', $data['item_consume'], $temp)) {
			$item_id = $map[$temp[1]];
			$item_cost = intval($temp[2]);
			if(!empty($item_id) && !empty($item_cost)) {
				$data['item_id'] = $item_id;
				$data['item_cost'] = $item_cost;
			} else
				throw new \Exception('Parsing error on item consume: '.$param);
		} else
			throw new \Exception('Parsing error on item consume: '.$param);

	unset($data['skill_name'], $data['skill_id'], $data['item_consume']);

	return $data;
}

// Find broken names: select id, level, name from skill where id in (select id from skill group by id having count(distinct name) > 1) group by id, name;
function process_skills(\PDO $db, $s_names, $s_data, $map) {
	echo 'Processing skills.'.PHP_EOL;
	if($s_names) {
		$db->query('DELETE FROM skill');

		$file = new Script($s_names, 1);
		while(!is_null($line = $file->line())) {
			$temp = null;
			if(preg_match('/^(\d+)\s+(\d+)\s+a,(.+?)\\\0/', $line, $temp)) {
				$query = $db->prepare('INSERT INTO skill(id, level, name, type, harmful) values(:id, :level, :name, -1, -9)');
				$query->bindValue(':id', $temp[1]);
				$query->bindValue(':level', $temp[2]);
				$query->bindValue(':name', $temp[3]);
				$query->execute();
			}
			else
				echo 'Parsing error on line:'.PHP_EOL.$line.PHP_EOL;
		}

		$db->query('UPDATE skill SET name = \'Summon Storm Cubic\' WHERE id = 10');
		$db->query('UPDATE skill SET name = \'Hate\' WHERE id = 28');
		$db->query('UPDATE skill SET name = \'Special Ability: Magic Damage\' WHERE id = 3049');
		$db->query('UPDATE skill SET name = \'Cubic Drain\' WHERE id = 4049');
		$db->query('UPDATE skill SET name = \'Mechanical Cannon\' WHERE id = 4068');
		$db->query('UPDATE skill SET name = \'Steal Blood\' WHERE id = 4260');
	}

	if($s_data) {
		$file = new Script($s_data);
		while(!is_null($line = $file->line())) {
			if($data = parse_skill($line, $map)) {
				$mp_cost = intval($data['mp_consume1'] ?? 0) + intval($data['mp_consume2'] ?? 0);
				$hp_cost = intval($data['hp_consume'] ?? 0);
				$item_id = $data['item_id'] ?? 0;
				$item_cost = $data['item_cost'] ?? 0;
				switch(trim($data['operate_type'] ?? '')) {
					case 'A1': case 'A2': case 'A3': $type = 0; break;
					case 'T': $type = 1; break;
					default: $type = 2; break;
				}

				$query = $db->prepare("
					UPDATE skill SET
						type = :type,
						mp_cost = :mp_cost,
						hp_cost = :hp_cost,
						item_id = :item_id,
						item_cost = :item_cost
					WHERE id = :id AND level = :level");
				$query->bindValue(':id', $data['id']);
				$query->bindValue(':level', $data['level']);
				$query->bindValue(':type', $type);
				// $query->bindValue(':harmful', $type == 0 && trim($data['next_action'] ?? '') == 'attack' ? 1 : 0);
				if($mp_cost > 0)
					$query->bindValue(':mp_cost', $mp_cost);
				else
					$query->bindValue(':mp_cost', null, \PDO::PARAM_NULL);
				if($hp_cost > 0)
					$query->bindValue(':hp_cost', $hp_cost);
				else
					$query->bindValue(':hp_cost', null, \PDO::PARAM_NULL);
				if($item_id > 0 && $item_cost > 0) {
					$query->bindValue(':item_id', $item_id);
					$query->bindValue(':item_cost', $item_cost);
				} else {
					$query->bindValue(':item_id', null, \PDO::PARAM_NULL);
					$query->bindValue(':item_cost', null, \PDO::PARAM_NULL);
				}
				$query->execute();
			}
			else
				throw new \Exception('Parsing error on line: '.$line);
		}

		$db->exec('UPDATE skill SET harmful = 0 WHERE id IN ('.implode(', ', [
			4, 8, 10, 13, 21, 22, 25, 27, 33, 34,
			42, 44, 45, 46, 50, 58, 61, 67, 69, 72,
			75, 76, 77, 78, 80, 82, 83, 86, 87, 88,
			91, 94, 99, 104, 109, 110, 111, 112, 121, 123,
			130, 131, 139, 176, 181, 230, 247, 262, 264, 265,
			266, 267, 268, 269, 270, 271, 272, 273, 274, 275,
			276, 277, 278, 282, 283, 287, 292, 297, 298, 299,
			301, 303, 304, 305, 306, 307, 308, 309, 310, 311,
			313, 323, 324, 326, 327, 341, 349, 350, 351, 355,
			356, 357, 359, 360, 363, 364, 365, 366, 367, 1002,
			1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012,
			1013, 1015, 1016, 1018, 1020, 1027, 1032, 1033, 1035, 1036,
			1040, 1043, 1044, 1045, 1047, 1048, 1050, 1059, 1062, 1068,
			1073, 1077, 1078, 1085, 1086, 1087, 1111, 1126, 1127, 1128,
			1129, 1139, 1140, 1141, 1144, 1145, 1146, 1154, 1157, 1182,
			1189, 1191, 1204, 1216, 1217, 1218, 1219, 1225, 1226, 1227,
			1228, 1229, 1232, 1238, 1240, 1242, 1243, 1249, 1250, 1251,
			1252, 1253, 1254, 1255, 1256, 1257, 1258, 1259, 1260, 1261,
			1268, 1271, 1276, 1277, 1278, 1279, 1280, 1281, 1282, 1284,
			1285, 1286, 1287, 1299, 1300, 1301, 1303, 1304, 1305, 1306,
			1307, 1308, 1309, 1310, 1311, 1312, 1313, 1314, 1321, 1322,
			1323, 1324, 1325, 1326, 1327, 1328, 1329, 1330, 1331, 1332,
			1333, 1334, 1335, 1346, 1347, 1348, 1349, 1352, 1353, 1354,
			1355, 1356, 1357, 1362, 1363, 1364, 1365, 2001, 2002, 2003,
			2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
			2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
			2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033,
			2034, 2035, 2036, 2037, 2038, 2039, 2040, 2041, 2042, 2043,
			2044, 2045, 2046, 2047, 2048, 2049, 2050, 2051, 2052, 2053,
			2054, 2055, 2056, 2057, 2058, 2059, 2060, 2061, 2062, 2063,
			2064, 2066, 2067, 2068, 2069, 2070, 2071, 2072, 2073, 2076,
			2077, 2078, 2079, 2080, 2081, 2082, 2083, 2084, 2085, 2086,
			2087, 2088, 2089, 2090, 2091, 2092, 2093, 2094, 2095, 2098,
			2099, 2100, 2101, 2102, 2103, 2104, 2105, 2106, 2107, 2108,
			2109, 2110, 2111, 2112, 2113, 2114, 2115, 2116, 2117, 2118,
			2119, 2120, 2121, 2122, 2123, 2124, 2125, 2126, 2127, 2128,
			2129, 2130, 2131, 2132, 2133, 2134, 2135, 2136, 2137, 2138,
			2139, 2140, 2141, 2142, 2143, 2144, 2145, 2146, 2147, 2148,
			2149, 2150, 2151, 2152, 2153, 2154, 2155, 2156, 2157, 2158,
			2159, 2160, 2161, 2162, 2163, 2164, 2165, 2166, 2167, 2168,
			2169, 2170, 2171, 2172, 2173, 2174, 2175, 2176, 2177, 2178,
			2179, 2180, 2181, 2182, 2183, 2184, 2185, 2186, 2187, 2190,
			2191, 2192, 2193, 2194, 2195, 2196, 2197, 2198, 2199, 2200,
			2201, 2202, 2203, 2204, 2205, 2206, 2207, 2208, 2209, 2210,
			2211, 2212, 2213, 2214, 2215, 2216, 2217, 2218, 2219, 2220,
			2221, 2222, 2223, 2224, 2225, 2226, 2227, 2228, 2230, 2231,
			2232, 2233, 3596, 3598, 4020, 4024, 4025, 4027, 4028, 4029,
			4030, 4031, 4044, 4048, 4051, 4065, 4074, 4080, 4089, 4090,
			4091, 4092, 4094, 4096, 4097, 4099, 4103, 4115, 4125, 4126,
			4133, 4135, 4140, 4161, 4163, 4173, 4174, 4175, 4176, 4209,
			4210, 4211, 4212, 4213, 4214, 4222, 4223, 4224, 4227, 4235,
			4238, 4239, 4240, 4241, 4242, 4245, 4262, 4263, 4264, 4265,
			4266, 4317, 4318, 4322, 4323, 4324, 4325, 4326, 4327, 4328,
			4329, 4330, 4331, 4338, 4339, 4340, 4341, 4342, 4343, 4344,
			4345, 4346, 4347, 4348, 4349, 4350, 4351, 4352, 4353, 4354,
			4355, 4356, 4357, 4358, 4359, 4360, 4364, 4365, 4366, 4367,
			4368, 4369, 4370, 4371, 4372, 4373, 4374, 4375, 4378, 4380,
			4384, 4385, 4386, 4387, 4391, 4392, 4393, 4394, 4395, 4396,
			4397, 4398, 4399, 4400, 4401, 4402, 4403, 4404, 4405, 4406,
			4513, 4514, 4516, 4522, 4523, 4524, 4527, 4528, 4530, 4531,
			4546, 4548, 4549, 4550, 4556, 4557, 4558, 4559, 4575, 4576,
			4585, 4588, 4595, 4601, 4608, 4609, 4610, 4611, 4613, 4617,
			4619, 4626, 4627, 4628, 4631, 4632, 4633, 4634, 4635, 4636,
			4637, 4638, 4639, 4644, 4645, 4646, 4647, 4648, 4650, 4651,
			4652, 4671, 4672, 4680, 4691, 4692, 4693, 4698, 4699, 4700,
			4701, 4702, 4703, 4704, 4707, 4711, 4713, 4717, 4718, 4779,
			4780, 4781, 4782, 4783, 4784, 4785, 4786, 4787, 4788, 7001,
			7002, 7003, 7004, 7005, 7006, 7029, 7030, 7031, 7032,
		]).')');

		$db->exec('UPDATE skill SET harmful = 1 WHERE id IN ('.implode(', ', [
			1, 3, 5, 6, 7, 9, 16, 17, 19, 24,
			29, 30, 35, 36, 48, 49, 54, 56, 70, 81,
			84, 92, 95, 96, 97, 98, 100, 101, 102, 103,
			105, 115, 116, 120, 122, 127, 129, 190, 223, 245,
			255, 260, 261, 263, 279, 280, 281, 284, 289, 314,
			315, 320, 321, 342, 343, 344, 345, 346, 347, 352,
			353, 354, 358, 361, 362, 369, 1056, 1064, 1069, 1071,
			1072, 1074, 1083, 1090, 1092, 1095, 1096, 1097, 1099, 1100,
			1101, 1102, 1104, 1105, 1107, 1108, 1147, 1148, 1155, 1159,
			1160, 1164, 1167, 1168, 1169, 1170, 1171, 1172, 1174, 1175,
			1176, 1177, 1178, 1181, 1183, 1184, 1201, 1206, 1208, 1209,
			1210, 1220, 1222, 1223, 1224, 1230, 1231, 1233, 1234, 1235,
			1236, 1237, 1239, 1244, 1245, 1246, 1247, 1248, 1263, 1264,
			1265, 1266, 1267, 1269, 1272, 1274, 1275, 1288, 1289, 1290,
			1291, 1292, 1293, 1294, 1295, 1296, 1298, 1336, 1337, 1338,
			1339, 1340, 1341, 1342, 1343, 1344, 1345, 1350, 1351, 1358,
			1359, 1360, 1361, 1366, 1367, 2074, 2097, 3005, 3016, 3020,
			3021, 3022, 3024, 3025, 3039, 3040, 3041, 3049, 3052, 3053,
			3054, 3055, 3059, 3060, 3061, 3062, 3070, 3074, 3075, 3078,
			3079, 3571, 3574, 3577, 3579, 3584, 3586, 3588, 3590, 3592,
			3594, 4001, 4002, 4017, 4018, 4019, 4026, 4032, 4033, 4034,
			4035, 4036, 4037, 4038, 4039, 4040, 4041, 4042, 4043, 4046,
			4047, 4049, 4050, 4052, 4053, 4054, 4055, 4063, 4064, 4066,
			4067, 4068, 4069, 4070, 4072, 4073, 4075, 4076, 4077, 4078,
			4079, 4081, 4082, 4083, 4087, 4088, 4098, 4100, 4101, 4102,
			4104, 4105, 4106, 4107, 4108, 4109, 4110, 4111, 4112, 4113,
			4114, 4117, 4118, 4119, 4120, 4124, 4127, 4128, 4129, 4130,
			4131, 4132, 4134, 4136, 4137, 4139, 4141, 4142, 4143, 4144,
			4145, 4146, 4147, 4148, 4149, 4150, 4151, 4152, 4153, 4154,
			4155, 4156, 4157, 4158, 4159, 4160, 4162, 4164, 4165, 4166,
			4167, 4168, 4169, 4170, 4171, 4172, 4177, 4178, 4179, 4180,
			4181, 4182, 4183, 4184, 4185, 4186, 4187, 4188, 4189, 4190,
			4191, 4192, 4193, 4194, 4195, 4196, 4197, 4198, 4199, 4200,
			4201, 4202, 4203, 4204, 4205, 4206, 4207, 4208, 4215, 4216,
			4217, 4218, 4219, 4220, 4221, 4228, 4229, 4230, 4231, 4232,
			4234, 4236, 4237, 4243, 4244, 4247, 4248, 4249, 4250, 4251,
			4252, 4253, 4254, 4255, 4257, 4258, 4259, 4260, 4261, 4289,
			4314, 4315, 4316, 4319, 4320, 4321, 4334, 4361, 4362, 4363,
			4377, 4382, 4383, 4495, 4496, 4515, 4526, 4529, 4533, 4534,
			4535, 4536, 4537, 4538, 4539, 4540, 4541, 4544, 4547, 4551,
			4552, 4553, 4554, 4560, 4561, 4562, 4563, 4564, 4565, 4566,
			4567, 4568, 4569, 4570, 4571, 4572, 4573, 4574, 4577, 4578,
			4579, 4580, 4581, 4582, 4583, 4584, 4586, 4587, 4589, 4590,
			4591, 4592, 4593, 4594, 4596, 4597, 4598, 4599, 4600, 4602,
			4603, 4604, 4605, 4606, 4607, 4612, 4614, 4615, 4616, 4618,
			4620, 4621, 4622, 4623, 4624, 4625, 4629, 4630, 4640, 4641,
			4643, 4649, 4654, 4655, 4656, 4657, 4658, 4659, 4660, 4661,
			4662, 4664, 4665, 4666, 4667, 4668, 4669, 4670, 4673, 4674,
			4675, 4676, 4677, 4678, 4681, 4682, 4683, 4684, 4685, 4686,
			4687, 4688, 4689, 4690, 4694, 4695, 4696, 4705, 4706, 4708,
			4709, 4710, 4712, 4719, 4720, 4721, 4722, 4723, 4724, 4725,
			4726, 4727, 4728, 4729, 4730, 4731, 4732, 4733, 4734, 4735,
			4736, 4737, 4738, 4739, 4740, 4741, 4742, 4743, 4744, 4745,
			4746, 4747, 4748, 4749, 4750, 4751, 4752, 4753, 4754, 4755,
			4756, 4757, 4758, 4759, 4760, 4761, 4762, 4763, 4764, 4765,
			4766, 4767, 4768, 4769, 4770, 4771, 4772, 4773, 4774, 4775,
			4776, 4777, 4778, 7007,
		]).')');

		$db->exec('UPDATE skill SET harmful = -1 WHERE id IN ('.implode(', ', [
			2, 11, 12, 15, 18, 28, 51, 65, 106, 107,
			246, 254, 286, 302, 325, 348, 368, 1028, 1031, 1034,
			1042, 1049, 1075, 1151, 1156, 1163, 1213, 1273, 2065, 2075,
			2096, 2188, 2189, 2229, 3581, 4123, 4138, 4653, 4663, 4697,
		]).')');
	}
}

function parse_item($line) {
	$temp = preg_split('/\t+/', $line);
	if(empty($temp))
		return null;
	if(end($temp) != 'item_end' || reset($temp) != 'item_begin')
		return null;

	$data = [
		'id' => $temp[2],
		'name' => substr($temp[3], 1, -1),
		'type' => $temp[1],
	];
	foreach(array_slice($temp, 4, -1) as $param) {
		list($key, $value) = explode('=', $param, 2);
		if(!empty($key) && trim($value) !== '')
			$data[$key] = trim($value);
		else
			throw new \Exception('Parsing error on parameter: '.$param);
	}

	//if($temp[1] != $data['item_type'])
	//	printf("Warning! Item type mismatch: %d %s [%s <-> %s].\n", $data['id'], $data['name'], $temp[1], $data['item_type']);

	return $data;
}
function dump_item($data, $slot) {
	printf("Warning! Unexpected item type: %d [%s] %s = %s.\n", $data['id'], $data['name'], $slot, $data[$slot]);
}
function process_items(\PDO $db, $s_names, $s_data) {
	echo 'Processing items.'.PHP_EOL;
	if($s_names) {
		$db->query('DELETE FROM item');

		$file = new Script($s_names, 1);
		while(!is_null($line = $file->line())) {
			$temp = null;
			if(preg_match('/^(\d+)\s+([^\t]+)/', $line, $temp)) {
				$query = $db->prepare('INSERT INTO item(id, name, type, grade, stack) values(:id, :name, -9, -1, -1)');
				$query->bindValue(':id', $temp[1]);
				$query->bindValue(':name', $temp[2]);
				$query->execute();
			}
			else
				echo 'Parsing error on line:'.PHP_EOL.$line.PHP_EOL;
		}
	}

	if($s_data) {
		$file = new Script($s_data);
		while(!is_null($line = $file->line())) {
			if($data = parse_item($line)) {
				if($data['equip_pet'] != '{0}')
					$type = 0;
				else
					switch($data['item_type']) {
						case 'weapon':
							switch($data['weapon_type'] ?? null) {
								case 'sword': $type = 100; break;
								case 'blunt': $type = 101; break;
								case 'dagger': $type = 102; break;
								case 'bow': $type = 104; break;
								case 'pole': $type = 103; break;
								case 'dualfist': $type = 106; break;
								case 'dual': $type = 105; break;
								case 'etc': case 'fishingrod': $type = 107; break;
								default:
									dump_item($data, 'weapon_type');
									continue 3;
							}
						break;
						case 'armor':
							switch($data['armor_type'] ?? null) {
								case 'heavy':
									switch($data['slot_bit_type'] ?? null) {
										case '{onepiece}': $type = 210; break;
										case '{chest}': $type = 211; break;
										case '{legs}': $type = 212; break;
										default:
											dump_item($data, 'slot_bit_type');
											continue 4;
									}
								break;
								case 'light':
									switch($data['slot_bit_type'] ?? null) {
										case '{onepiece}': $type = 220; break;
										case '{chest}': $type = 221; break;
										case '{legs}': $type = 222; break;
										default:
											dump_item($data, 'slot_bit_type');
											continue 4;
									}
								break;
								case 'magic':
									switch($data['slot_bit_type'] ?? null) {
										case '{onepiece}': case '{alldress}': $type = 230; break;
										case '{chest}': $type = 231; break;
										case '{legs}': $type = 232; break;
										default:
											dump_item($data, 'slot_bit_type');
											continue 4;
									}
								break;
								default:
									switch($data['slot_bit_type'] ?? null) {
										case '{head}': $type = 200; break;
										case '{gloves}': $type = 201; break;
										case '{feet}': $type = 202; break;
										case '{hair}': $type = 203; break;
										case '{lhand}': $type = 300; break;
										case '{underwear}': case '{back}': $type = 0; break;
										default:
											dump_item($data, 'slot_bit_type');
											continue 4;
									}
							}
						break;
						case 'accessary':
							switch($data['slot_bit_type'] ?? null) {
								case '{neck}': $type = 400; break;
								case '{rear;lear}': $type = 401; break;
								case '{rfinger;lfinger}': $type = 402; break;
								default:
									dump_item($data, 'slot_bit_type');
									continue 3;
							}
						break;
						case 'etcitem':
							switch($data['etcitem_type'] ?? null) {
								case 'arrow': $type = 150; break;
								case 'potion': $type = 1; break;
								case 'scroll': $type = 2; break;
								case 'recipe': $type = 3; break;
								case 'scrl_enchant_wp': case 'bless_scrl_enchant_wp': $type = 50; break;
								case 'scrl_enchant_am': case 'bless_scrl_enchant_am': $type = 51; break;
								case 'pet_collar':
									switch($data['id']) {
										case 2375: $type = 500; break;
										case 3500: case 3501: case 3502: $type = 510; break;
										case 4422: case 4423: case 4424: $type = 511; break;
										case 6648: $type = 530; break;
										case 6649: $type = 540; break;
										case 6650: $type = 520; break;
										case 4425: $type = 550; break;
										default:
											dump_item($data, 'etcitem_type');
											continue 4;
									}
								break;
								default: $type = 0;
							}
						break;
						case 'questitem': $type = -1; break;
						default: $type = 0; break;
					}

				switch($data['crystal_type'] ?? null) {
					case 'none': $grade = 0; break;
					case 'd': $grade = 1; break;
					case 'c': $grade = 2; break;
					case 'b': $grade = 3; break;
					case 'a': $grade = 4; break;
					case 's': $grade = 5; break;
					default: throw new \Exception('Unexpected crystal type on line: '.$line);
				}

				switch($data['consume_type'] ?? null) {
					case 'consume_type_normal': $stack = 1; break;
					case 'consume_type_stackable': $stack = intval($data['maximum_count'] ?? 0) ?: 20; break;
					case 'consume_type_asset': $stack = 0; break;
					default: throw new \Exception('Unexpected consume type on line: '.$line);
				}

				$query = $db->prepare('UPDATE item SET type = :type, grade = :grade, stack = :stack WHERE id = :id');
				$query->bindValue(':id', $data['id']);
				$query->bindValue(':type', $type);
				$query->bindValue(':grade', $grade);
				$query->bindValue(':stack', $stack);
		 		$query->execute();
			} elseif(preg_match('/^\s*set_begin\s/', $line))
				continue;
		 	else
		 		throw new \Exception('Parsing error on line: '.$line);
		}

		$db->query('DELETE FROM item WHERE type < -1 OR grade < 0 OR stack < 0');
	}
}


$db = new \PDO('sqlite:/dev/shm/apf.db');
$db->query('PRAGMA foreign_keys = ON');

$item_map = item_map($db, 'item_pch.txt');

process_npcs($db, 'npcname-e.txt', 'npcdata.txt');
process_skills($db, 'skillname-e.txt', 'skilldata.txt', $item_map);
process_items($db, 'itemname-e.txt', 'itemdata.txt');

$db->query('VACUUM');
echo 'Done'.PHP_EOL;

