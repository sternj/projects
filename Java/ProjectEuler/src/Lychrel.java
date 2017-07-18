
public class Lychrel 
{
	public static void main(String[] args)
	{
		System.out.print(isPal(6556));
	}
	public static boolean isPal(int j)
	{
		String str = j+"";
		for(int i = 0; i < (int)(str.length()-1)/2; i++)
		{
			System.out.println(i);
			
			if(str.charAt(i) != str.charAt(str.length()-1-i))
			{
				return false;
			}
		}
		return true;
	}
}
